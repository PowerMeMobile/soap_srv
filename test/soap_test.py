# -*- coding: utf-8 -*-

import sys
reload(sys)
sys.setdefaultencoding('utf-8')

import pytest

import os
import requests
import xmltodict
import time as time

SOAP11 = 'soapenv'
SOAP12 = 'soap12env'

SOAP_HOST = os.getenv('SOAP_HOST')
if SOAP_HOST == None or SOAP_HOST == '':
    SOAP_HOST = '127.0.0.1'

SOAP_PORT = os.getenv('SOAP_PORT')
if SOAP_PORT == None or SOAP_PORT == '':
    SOAP_PORT = '8088'

SMPPSIM_HOST = os.getenv('SMPPSIM_HOST')
if SMPPSIM_HOST == None or SMPPSIM_HOST == '':
    SMPPSIM_HOST = ONEAPI_HOST

SMPPSIM_PORT = os.getenv('SMPPSIM_PORT')
if SMPPSIM_PORT == None or SMPPSIM_PORT == '':
    SMPPSIM_PORT = '8071'

WSDL = 'http://{0}:{1}/bmsgw/soap/messenger.asmx?WSDL'.format(SOAP_HOST, SOAP_PORT)
SMPPSIM_SERVER = 'http://{0}:{1}'.format(SMPPSIM_HOST, SMPPSIM_PORT)

CUSTOMER_ID = 10003
USER_ID     = 'user'
PASSWORD    = 'password'
BAD_PASSWORD = 'intentionally wrong password'

USER = {
    'CustomerID':CUSTOMER_ID,
    'Name':USER_ID,
    'Language':'en',
    'Password':PASSWORD
}

BAD_USER = {
    'CustomerID':CUSTOMER_ID,
    'Name':USER_ID,
    'Language':'en',
    'Password':BAD_PASSWORD
}

ORIGINATOR = '375296660003'
SHORT_CODE = '0031'
RECIPIENT = '375296543210'
BLACKLISTED_RECIPIENT = '375296666666'
BAD_RECIPIENT = '888999999999'
RECIPIENT_BASE64     = 'Mzc1Mjk2NTQzMjEw'
BAD_RECIPIENT_BASE64 = 'ODg4OTk5OTk5OTk5'

TRANSACTION_ID = '85ccccbf-f854-4898-86b1-5072d3e33da1'
BAD_TRANSACTION_ID = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'

@pytest.fixture(scope="module",
                params=[SOAP11, SOAP12])
def client(request):
    from pysimplesoap.client import SoapClient

    wsdl = WSDL
    soap_ns = request.param

    client = SoapClient(wsdl=wsdl, soap_ns=soap_ns, trace=False)

    # fix content type for soap12
    if soap_ns == 'soap12env':
        client.http_headers = {
            'Content-type':'application/soap+xml;charset=utf-8'
        }

    # Statistics and Details parsers
    # workaround of <s:sequence><s:any/></s:sequence>

    # return Statistics and Details as embedded XML
    #client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = None
    #client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Details'] = None

    # SMSC_DELIVERED, 3e231a28-cdf2-4d95-ac2e-a8cd1e0a0a0a - TempFail_WillRetry
    statistics = {
        'submitted':int,
        'sent':int,
        'failed':int,
        'delivered':int,
        'expired':int,
        'deleted':int,
        'undeliverable':int,
        'accepted':int,
        'unknown':int,
        'rejected':int,
        'SMSC_DELIVERED':int,
        'SMSC_ACCEPTED':int
    }
    client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = {'statistics':statistics}

    numbers = [{'number':str, 'TimeStamp':str, 'StatusU':str}]
    details = {
        'submitted':numbers,
        'sent':numbers,
        'failed':numbers,
        'delivered':numbers,
        'expired':numbers,
        'deleted':numbers,
        'undeliverable':numbers,
        'accepted':numbers,
        'unknown':numbers,
        'rejected':numbers,
        'SMSC_DELIVERED':numbers,
        'SMSC_ACCEPTED':numbers
    }
    client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Details'] = {'details':details}

    return client

#
# Utils
#

def send_inbound_via_smppsim(src_addr, dst_addr, message):
    url = SMPPSIM_SERVER + '/inject_mo'
    params = {'short_message':message,
              'source_addr':src_addr, 'source_addr_ton':'1', 'source_addr_npi':'1',
              'destination_addr':dst_addr, 'dest_addr_ton':'6', 'dest_addr_npi':'0'}
    req = requests.get(url, params=params)
    assert req.status_code == 200

def strip_soap_body(res, method, tag):
    # ugly workaround for <s:sequence><s:any/></s:sequence>, but works :(
    # strip 'soap[12]:Envelop' and 'soap[12]:Body'
    xml = xmltodict.parse(res[method + 'Result'][tag].as_xml()).items()[0][1].items()[3][1]
    return  xml[method + 'Response'][method + 'Result'][tag]

def from_hex_utf16be(string):
    return string.decode('hex').decode('utf-16be')

#
# Authenticate
#

def test_Authenticate_bad_user_fail(client):
    res = client.Authenticate(user=BAD_USER)
    assert res['AuthenticateResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['AuthenticateResult']['Originators'] == []
    assert res['AuthenticateResult']['NetPoints'] == '0'
    assert res['AuthenticateResult']['CreditSMS'] == None
    assert res['AuthenticateResult']['CustomerID'] == -1

def test_Authenticate_succ(client):
    res = client.Authenticate(user=USER)
    assert res['AuthenticateResult']['Result'] == 'OK'
    assert res['AuthenticateResult']['Originators'] != []
    assert res['AuthenticateResult']['NetPoints'] == 'POSTPAID'
    assert res['AuthenticateResult']['CreditSMS'] == 'POSTPAID'
    assert res['AuthenticateResult']['CustomerID'] == CUSTOMER_ID

#
# KeepAlive
#

def test_KeepAlive_bad_user_fail(client):
    res = client.KeepAlive(user=BAD_USER)
    assert res['KeepAliveResult']['Result'] == '404.2 FAILURE (User is unknown)'

def test_KeepAlive_succ(client):
    res = client.KeepAlive(user=USER)
    assert res['KeepAliveResult']['Result'] == 'OK'

#
# SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def test_SendSms_bad_user_fail(client):
    res = client.SendSms(user=BAD_USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_bad_originator_fail(client):
    res = client.SendSms(user=USER, originator='', smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_no_recipient_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone='', messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == '600.4 Phone not specified'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_bad_recipient_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=BAD_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_empty_body_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'Message Content Is Empty'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_bad_defdate_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='2014-07-21', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_SendSms_defdate_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='20140721150000', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'OK'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendSmsResult']['TransactionID'] != None

def test_SendSms_latin_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'OK'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendSmsResult']['TransactionID'] != None

def test_SendSms_arabic_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='بطاقتك مسروقة', recipientPhone=RECIPIENT, messageType='ArabicWithLatinNumbers', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'OK'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendSmsResult']['TransactionID'] != None

#
# SendBinarySms
#

def test_SendBinarySms_bad_user_fail(client):
    res = client.SendBinarySms(user=BAD_USER, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_bad_originator_fail(client):
    res = client.SendBinarySms(user=USER, originator='', binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_no_recipient_fail(client):
    res = client.SendBinarySms(user=USER, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone='', defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == '600.4 Phone not specified'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_bad_recipient_fail(client):
    res = client.SendBinarySms(user=USER, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=BAD_RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_empty_body_fail(client):
    res = client.SendBinarySms(user=USER, originator=ORIGINATOR, binaryBody='', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == 'Message Content Is Empty'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_bad_defdate_fail(client):
    res = client.SendBinarySms(user=USER, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='2014-07-21', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == '0'
    assert res['SendBinarySmsResult']['TransactionID'] == None

def test_SendBinarySms_succ(client):
    res = client.SendBinarySms(user=USER, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendBinarySmsResult']['Result'] == 'OK'
    assert res['SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['SendBinarySmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendBinarySmsResult']['TransactionID'] != None

#
# SendServiceSms
#

def test_SendServiceSms_bad_password_fail(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='http://google.com', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == '0'
    assert res['SendServiceSmsResult']['TransactionID'] == None

def test_SendServiceSms_bad_originator_fail(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator='', serviceName='google', serviceUrl='http://google.com', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == '0'
    assert res['SendServiceSmsResult']['TransactionID'] == None

def test_SendServiceSms_no_recipient_fail(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='http://google.com', recipientPhone='', messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == '600.4 Phone not specified'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == '0'
    assert res['SendServiceSmsResult']['TransactionID'] == None

def test_SendServiceSms_bad_recipient_fail(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='http://google.com', recipientPhone=BAD_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == '0'
    assert res['SendServiceSmsResult']['TransactionID'] == None

def DISABLED_test_SendServiceSms_empty_service_name_succ(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='', serviceUrl='http://google.com', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == 'OK'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendServiceSmsResult']['TransactionID'] != None

def DISABLED_test_SendServiceSms_empty_service_url_succ(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == 'OK'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendServiceSmsResult']['TransactionID'] != None

def test_SendServiceSms_bad_defdate_fail(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='http://google.com', recipientPhone=RECIPIENT, messageType='Latin', defDate='2014-07-21', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == '0'
    assert res['SendServiceSmsResult']['TransactionID'] == None

def test_SendServiceSms_succ(client):
    res = client.SendServiceSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, serviceName='google', serviceUrl='http://google.com', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendServiceSmsResult']['Result'] == 'OK'
    assert res['SendServiceSmsResult']['RejectedNumbers'] == []
    assert res['SendServiceSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendServiceSmsResult']['TransactionID'] != None

#
# SendSms2
#

def test_SendSms2_bad_user_fail(client):
    res = client.SendSms2(user=BAD_USER, originator=ORIGINATOR, smsText='Hello', recipientPhonesFile=RECIPIENT_BASE64, messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_bad_originator_fail(client):
    res = client.SendSms2(user=USER, originator='', smsText='Hello', recipientPhonesFile=RECIPIENT_BASE64, messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_no_recipient_fail(client):
    res = client.SendSms2(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhonesFile='', messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == '600.4 Phone not specified'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_bad_recipient_fail(client):
    res = client.SendSms2(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhonesFile=BAD_RECIPIENT_BASE64, messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_empty_body_fail(client):
    res = client.SendSms2(user=USER, originator=ORIGINATOR, smsText='', recipientPhonesFile=RECIPIENT_BASE64, messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == 'Message Content Is Empty'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_bad_defdate_fail(client):
    res = client.SendSms2(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhonesFile=RECIPIENT_BASE64, messageType='Latin', defDate='2014-07-21', flash=False)
    assert res['SendSms2Result']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == '0'
    assert res['SendSms2Result']['TransactionID'] == None

def test_SendSms2_succ(client):
    res = client.SendSms2(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhonesFile=RECIPIENT_BASE64, messageType='Latin', defDate='', flash=False)
    assert res['SendSms2Result']['Result'] == 'OK'
    assert res['SendSms2Result']['RejectedNumbers'] == []
    assert res['SendSms2Result']['NetPoints'] == 'POSTPAID'
    assert res['SendSms2Result']['TransactionID'] != None

#
# GetSmsStatus
#

def test_GetSmsStatus_bad_user_fail(client):
    res = client.GetSmsStatus(user=BAD_USER, transactionID='', detailed=False)
    assert res['GetSmsStatusResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['GetSmsStatusResult']['NetPoints'] == '0'

def test_GetSmsStatus_empty_transaction_id_fail(client):
    res = client.GetSmsStatus(user=USER, transactionID='', detailed=False)
    assert res['GetSmsStatusResult']['Result'] == "605.7 The action you requested cannot be performed, because one of your the required request parameters ('TransactionID') was not supplied."
    assert res['GetSmsStatusResult']['NetPoints'] == '0'

def test_GetSmsStatus_wrong_transaction_id_fail(client):
    res = client.GetSmsStatus(user=USER, transactionID=BAD_TRANSACTION_ID, detailed=False)
    assert res['GetSmsStatusResult']['Result'] == 'SMS ID for status request is incorrect or not specified'
    assert res['GetSmsStatusResult']['NetPoints'] == '0'

def test_GetSmsStatus_detailed_false_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['SendSmsResult']['TransactionID']
    time.sleep(2)
    res = client.GetSmsStatus(user=USER, transactionID=trans_id, detailed=False)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    assert res['GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1

def test_GetSmsStatus_detailed_true_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['SendSmsResult']['TransactionID']
    time.sleep(2)
    res = client.GetSmsStatus(user=USER, transactionID=trans_id, detailed=True)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    assert res['GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1
    ## [{'StatusU': u'00640065006c006900760065007200650064'}, {'number': u'375296543210'}]
    StatusU = str(res['GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][0]['StatusU'])
    assert from_hex_utf16be(StatusU) == 'delivered'
    assert res['GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][1]['number'] == RECIPIENT

#
# InboxProcessing
#

def test_InboxProcessing_bad_operation_fail(client):
    res = client.InboxProcessing(user=USER, operation='bad-operation', messageId=None)
    info = strip_soap_body(res, 'InboxProcessing', 'inbox')
    assert info['result'] == 'Non-supported Inbox operation is specified!'

def test_InboxProcessing_stats_bad_password_fail(client):
    res = client.InboxProcessing(user=BAD_USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == '404.2 FAILURE (User is unknown)'

def test_InboxProcessing_list_all_bad_password_fail(client):
    res = client.InboxProcessing(user=BAD_USER, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == '404.2 FAILURE (User is unknown)'

def test_InboxProcessing_kill_old_bad_password_fail(client):
    res = client.InboxProcessing(user=BAD_USER, operation='kill-old', messageId=None)
    idel = strip_soap_body(res, 'InboxProcessing', 'inboxdel')
    assert idel['result'] == '404.2 FAILURE (User is unknown)'

def test_InboxProcessing_stats_empty_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['new'] == '0'
    assert iinfo['total'] == '0'

def test_InboxProcessing_stats_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, "Msg")
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_list_all_empty_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    res = client.InboxProcessing(user=USER, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_InboxProcessing_list_all_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_list_new_empty_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    res = client.InboxProcessing(user=USER, operation='list-new', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_InboxProcessing_list_new_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='list-new', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_fetch_all_empty_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    res = client.InboxProcessing(user=USER, operation='fetch-all', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_InboxProcessing_fetch_all_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='fetch-all', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_fetch_new_empty_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    res = client.InboxProcessing(user=USER, operation='fetch-new', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_InboxProcessing_fetch_new_succ(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='fetch-new', messageId=None)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_fetch_id(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'InboxProcessing', 'inboxlist')['message']
    msg_id = msg['@id']

    res = client.InboxProcessing(user=USER, operation='fetch-id', messageId=msg_id)
    ilist = strip_soap_body(res, 'InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_kill_all(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    info = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert info['total'] == '1'
    assert info['new'] == '1'

    res = client.InboxProcessing(user=USER, operation='kill-all', messageId=None)
    idel = strip_soap_body(res, 'InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_kill_id(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = client.InboxProcessing(user=USER, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'InboxProcessing', 'inboxlist')['message']
    msg_id = msg[0]['@id']

    res = client.InboxProcessing(user=USER, operation='kill-id', messageId=msg_id)
    idel = strip_soap_body(res, 'InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    info = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert info['total'] == '1'
    assert info['new'] == '1'

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

def test_InboxProcessing_kill_old(client):
    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = client.InboxProcessing(user=USER, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'InboxProcessing', 'inboxlist')['message']
    msg_id = msg[0]['@id']

    res = client.InboxProcessing(user=USER, operation='fetch-id', messageId=msg_id)

    res = client.InboxProcessing(user=USER, operation='kill-old', messageId=None)
    idel = strip_soap_body(res, 'InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.InboxProcessing(user=USER, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    client.InboxProcessing(user=USER, operation='kill-all', messageId=None)

#
# HTTP_Authenticate
#

def test_HTTP_Authenticate_bad_password_fail(client):
    res = client.HTTP_Authenticate(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['HTTP_AuthenticateResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['HTTP_AuthenticateResult']['Originators'] == []
    assert res['HTTP_AuthenticateResult']['NetPoints'] == '0'
    assert res['HTTP_AuthenticateResult']['CreditSMS'] == None
    assert res['HTTP_AuthenticateResult']['CustomerID'] == -1

def test_HTTP_Authenticate_succ(client):
    res = client.HTTP_Authenticate(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['HTTP_AuthenticateResult']['Result'] == 'OK'
    assert res['HTTP_AuthenticateResult']['Originators'] != []
    assert res['HTTP_AuthenticateResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CreditSMS'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CustomerID'] == CUSTOMER_ID

def test_HTTP_Authenticate_lower_succ(client):
    res = client.HTTP_Authenticate(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD.lower())
    assert res['HTTP_AuthenticateResult']['Result'] == 'OK'
    assert res['HTTP_AuthenticateResult']['Originators'] != []
    assert res['HTTP_AuthenticateResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CreditSMS'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CustomerID'] == CUSTOMER_ID

def test_HTTP_Authenticate_upper_succ(client):
    res = client.HTTP_Authenticate(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD.upper())
    assert res['HTTP_AuthenticateResult']['Result'] == 'OK'
    assert res['HTTP_AuthenticateResult']['Originators'] != []
    assert res['HTTP_AuthenticateResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CreditSMS'] == 'POSTPAID'
    assert res['HTTP_AuthenticateResult']['CustomerID'] == CUSTOMER_ID

#
# HTTP_KeepAlive
#

def test_HTTP_KeepAlive_bad_password_fail(client):
    res = client.HTTP_KeepAlive(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['HTTP_KeepAliveResult']['Result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_KeepAlive_fail(client):
    res = client.HTTP_KeepAlive(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['HTTP_KeepAliveResult']['Result'] == 'OK'

#
# HTTP_SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def test_HTTP_SendSms_bad_password_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_originator_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator='', smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_no_recipient_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone='', messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == '600.4 Phone not specified'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_recipient_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=BAD_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_empty_body_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'Message Content Is Empty'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_defdate_fail(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='2014-07-21', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendSmsResult']['TransactionID'] == None

def test_HTTP_SendSms_latin_succ(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'OK'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_SendSmsResult']['TransactionID'] != None

def test_HTTP_SendSms_arabic_succ(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='بطاقتك مسروقة', recipientPhone=RECIPIENT, messageType='ArabicWithLatinNumbers', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'OK'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_SendSmsResult']['TransactionID'] != None

#
# HTTP_SendBinarySms
#

def test_HTTP_SendBinarySms_bad_password_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_originator_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator='', binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_no_recipient_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone='', defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == '600.4 Phone not specified'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_recipient_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=BAD_RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_empty_body_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == 'Message Content Is Empty'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_defdate_fail(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='2014-07-21', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == '0'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_succ(client):
    res = client.HTTP_SendBinarySms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['HTTP_SendBinarySmsResult']['Result'] == 'OK'
    assert res['HTTP_SendBinarySmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendBinarySmsResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_SendBinarySmsResult']['TransactionID'] != None

#
# HTTP_GetSmsStatus
#

def test_HTTP_GetSmsStatus_bad_password_fail(client):
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, transactionID=TRANSACTION_ID, detailed=False)
    assert res['HTTP_GetSmsStatusResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_empty_transaction_id_fail(client):
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID='', detailed=False)
    assert res['HTTP_GetSmsStatusResult']['Result'] == "605.7 The action you requested cannot be performed, because one of your the required request parameters ('TransactionID') was not supplied."
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_wrong_transaction_id_fail(client):
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=BAD_TRANSACTION_ID, detailed=False)
    assert res['HTTP_GetSmsStatusResult']['Result'] == 'SMS ID for status request is incorrect or not specified'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_detailed_false_succ(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['HTTP_SendSmsResult']['TransactionID']
    time.sleep(2)
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=trans_id, detailed=False)
    assert res['HTTP_GetSmsStatusResult']['Result'] == 'OK'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1

def test_HTTP_GetSmsStatus_detailed_true_succ(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['HTTP_SendSmsResult']['TransactionID']
    time.sleep(2)
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=trans_id, detailed=True)
    assert res['HTTP_GetSmsStatusResult']['Result'] == 'OK'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1
    ## [{'StatusU': u'00640065006c006900760065007200650064'}, {'number': u'375296543210'}]
    StatusU = str(res['HTTP_GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][0]['StatusU'])
    assert from_hex_utf16be(StatusU) == 'delivered'
    assert res['HTTP_GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][1]['number'] == RECIPIENT

#
# HTTP_InboxProcessing
#

def test_HTTP_InboxProcessing_bad_operation_fail(client):
    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='bad-operation', messageId=None)
    info = strip_soap_body(res, 'HTTP_InboxProcessing', 'inbox')
    assert info['result'] == 'Non-supported Inbox operation is specified!'

def test_HTTP_InboxProcessing_stats_bad_password_fail(client):
    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_list_all_bad_password_fail(client):
    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_kill_old_bad_password_fail(client):
    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, operation='kill-old', messageId=None)
    idel = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxdel')
    assert idel['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_stats_empty_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['new'] == '0'
    assert iinfo['total'] == '0'

def test_HTTP_InboxProcessing_stats_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, "Msg")
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_list_all_empty_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_list_all_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-all', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_list_new_empty_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-new', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_list_new_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-new', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_fetch_all_empty_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-all', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_fetch_all_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-all', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_fetch_new_empty_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-new', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_fetch_new_succ(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-new', messageId=None)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_fetch_id(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')['message']
    msg_id = msg['@id']

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-id', messageId=msg_id)
    ilist = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_kill_all(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    info = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert info['total'] == '1'
    assert info['new'] == '1'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)
    idel = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_kill_id(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')['message']
    msg_id = msg[0]['@id']

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-id', messageId=msg_id)
    idel = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    info = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert info['total'] == '1'
    assert info['new'] == '1'

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

def test_HTTP_InboxProcessing_kill_old(client):
    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='list-new', messageId=None)
    msg = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxlist')['message']
    msg_id = msg[0]['@id']

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='fetch-id', messageId=msg_id)

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-old', messageId=None)
    idel = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxdel')
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='stats', messageId=None)
    iinfo = strip_soap_body(res, 'HTTP_InboxProcessing', 'inboxinfo')
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    client.HTTP_InboxProcessing(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, operation='kill-all', messageId=None)

#
# Blacklist
#

def test_blacklisted_recipient_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=BLACKLISTED_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_blacklisted_with_space_recipient_fail(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=' '+BLACKLISTED_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendSmsResult']['RejectedNumbers'] == []
    assert res['SendSmsResult']['NetPoints'] == '0'
    assert res['SendSmsResult']['TransactionID'] == None

def test_good_and_blacklisted_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT+', '+BLACKLISTED_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendSmsResult']['Result'] == 'OK'
    assert res['SendSmsResult']['RejectedNumbers'] == [{'string':BLACKLISTED_RECIPIENT}, {'string':None}]
    assert res['SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['SendSmsResult']['TransactionID'] != None
