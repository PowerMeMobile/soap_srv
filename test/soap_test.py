# setup python's virtualenv as described here
# https://gist.github.com/ten0s/98e7d88476ec75351d75

# $ . env/bin/activate
# $ py.test soap_test.py
# $ py.test soap_test.py -k test_Authenticate_bad_user_fail
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest

SOAP11 = 'soapenv'
SOAP12 = 'soap12env'

WSDL = 'http://localhost:8088/bmsgw/soap/messenger.asmx?WSDL'
#WSDL = 'http://mm.powermemobile.com/mm/soap/messenger.asmx?WSDL'

CUSTOMER_ID = 3
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

ORIGINATOR = 'SMS'
RECIPIENT = '375293615363'
BAD_RECIPIENT = '999999999999'
RECIPIENT_BASE64 = 'Mzc1Mjk2NTQzMjEw'
BAD_RECIPIENT_BASE64 = 'OTk5OTk5OTk5OTk5'

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
    statistics = {'submitted':int, 'sent':int, 'failed':int, 'delivered':int, 'expired':int, 'deleted':int, 'undeliverable':int, 'accepted':int, 'unknown':int, 'rejected':int, 'SMSC_DELIVERED':int}
    client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Statistics'] = {'statistics':statistics}

    numbers = [{'number':str, 'TimeStamp':str, 'StatusU':str}]
    details = {'submitted':numbers, 'sent':numbers, 'failed':numbers, 'delivered':numbers, 'expired':numbers, 'deleted':numbers, 'undeliverable':numbers, 'accepted':numbers, 'unknown':numbers, 'rejected':numbers, 'SMSC_DELIVERED':numbers}
    client.services['Messenger']['ports']['MessengerSoap']['operations']['GetSmsStatus']['output']['GetSmsStatusResponse']['GetSmsStatusResult']['Details'] = {'details':details}

    return client

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

def test_SendSms_succ(client):
    res = client.SendSms(user=USER, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
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

def test_SendServiceSms_bad_user_fail(client):
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
    res = client.GetSmsStatus(user=BAD_USER, transactionID=TRANSACTION_ID, detailed=False)
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
    res = client.GetSmsStatus(user=USER, transactionID=TRANSACTION_ID, detailed=False)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    assert res['GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1

def test_GetSmsStatus_detailed_false_succ(client):
    res = client.GetSmsStatus(user=USER, transactionID=TRANSACTION_ID, detailed=False)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    assert res['GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1

def test_GetSmsStatus_detailed_true_succ(client):
    res = client.GetSmsStatus(user=USER, transactionID=TRANSACTION_ID, detailed=True)
    assert res['GetSmsStatusResult']['Result'] == 'OK'
    assert res['GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1
    ## [{'StatusU': u'00640065006c006900760065007200650064'}, {'number': u'375296543210'}]
    assert res['GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][0]['StatusU'].lower() == '00640065006c006900760065007200650064' # 'Delivered'
    assert res['GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][1]['number'] == '375296543210'

#
# HTTP_Authenticate
#

def test_HTTP_Authenticate_bad_user_fail(client):
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

#
# HTTP_KeepAlive
#

def test_HTTP_KeepAlive_bad_user_fail(client):
    res = client.HTTP_KeepAlive(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['HTTP_KeepAliveResult']['Result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_KeepAlive_fail(client):
    res = client.HTTP_KeepAlive(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['HTTP_KeepAliveResult']['Result'] == 'OK'

#
# HTTP_SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def test_HTTP_SendSms_bad_user_fail(client):
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

def test_HTTP_SendSms_succ(client):
    res = client.HTTP_SendSms(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['HTTP_SendSmsResult']['Result'] == 'OK'
    assert res['HTTP_SendSmsResult']['RejectedNumbers'] == []
    assert res['HTTP_SendSmsResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_SendSmsResult']['TransactionID'] != None

#
# HTTP_SendBinarySms
#

def test_HTTP_SendBinarySms_bad_user_fail(client):
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

def test_HTTP_GetSmsStatus_bad_user_fail(client):
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
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=TRANSACTION_ID, detailed=False)
    assert res['HTTP_GetSmsStatusResult']['Result'] == 'OK'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1

def test_HTTP_GetSmsStatus_detailed_true_succ(client):
    res = client.HTTP_GetSmsStatus(customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=TRANSACTION_ID, detailed=True)
    assert res['HTTP_GetSmsStatusResult']['Result'] == 'OK'
    assert res['HTTP_GetSmsStatusResult']['NetPoints'] == 'POSTPAID'
    assert res['HTTP_GetSmsStatusResult']['Statistics']['statistics']['SMSC_DELIVERED'] == 1
    ## [{'StatusU': u'00640065006c006900760065007200650064'}, {'number': u'375296543210'}]
    assert res['HTTP_GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][0]['StatusU'].lower() == '00640065006c006900760065007200650064' # 'Delivered'
    assert res['HTTP_GetSmsStatusResult']['Details']['details']['SMSC_DELIVERED'][1]['number'] == '375296543210'
