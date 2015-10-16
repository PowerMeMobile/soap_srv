# -*- coding: utf-8 -*-

import pytest

import os
import requests
import xmltodict
import time as time
import datetime

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

SOAP_SERVER = 'http://{0}:{1}/bmsgw/soap/messenger.asmx'.format(SOAP_HOST, SOAP_PORT)
SMPPSIM_SERVER = 'http://{0}:{1}'.format(SMPPSIM_HOST, SMPPSIM_PORT)

CUSTOMER_ID = 10003
USER_ID = 'user'
USER_ID_NO_INBOX = 'user_no_inbox'
PASSWORD = 'password'
BAD_PASSWORD = 'intentionally wrong password'

ORIGINATOR = '375296660003'
SHORT_CODE = '0031'
RECIPIENT = '375296543210'
BAD_RECIPIENT = '999999999999'
RECIPIENT_BASE64 = 'Mzc1Mjk2NTQzMjEw'
BAD_RECIPIENT_BASE64 = 'OTk5OTk5OTk5OTk5'

BAD_TRANSACTION_ID = 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'

#
# Fixture
#

class HttpRequest:
    def __init__(self, method):
        self.method = method

    def make(self, url, params):
        req = None
        if self.method == 'GET':
            req = requests.get(url, params = params)
            print("{0}".format(req.request.url))
        elif self.method == 'POST':
            req = requests.post(url, data = params)
            print("{0} data: {1}".format(req.request.url, req.request.body))
        return xmltodict.parse(req.text)

@pytest.fixture(scope="function",
                params=["GET", "POST"])
def request(request):
    return HttpRequest(request.param)

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

def from_hex_utf16be(string):
    return string.decode('hex').decode('utf-16be')

#
# Requests
#

def authenticate(request, customerID, userName, userPassword):
    url = SOAP_SERVER + '/HTTP_Authenticate'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword}
    return request.make(url, params)

def keep_alive(request, customerID, userName, userPassword):
    url = SOAP_SERVER + '/HTTP_KeepAlive'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword}
    return request.make(url, params)

def send_sms(request, customerID, userName, userPassword, originator, smsText, recipientPhone, messageType, defDate, blink, flash, Private):
    url = SOAP_SERVER + '/HTTP_SendSms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword,
              'originator': originator, 'smsText': smsText, 'recipientPhone': recipientPhone, 'messageType': messageType,
              'defDate': defDate, 'blink': str(blink).lower(), 'flash': str(flash).lower(), 'Private': str(Private).lower()}
    return request.make(url, params)

def send_binary_sms(request, customerID, userName, userPassword, originator, binaryBody, recipientPhone, defDate, data_coding, esm_class, PID):
    url = SOAP_SERVER + '/HTTP_SendBinarySms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword,
              'originator': originator, 'binaryBody': binaryBody, 'recipientPhone': recipientPhone, 'defDate': defDate,
              'data_coding': data_coding, 'esm_class': esm_class, 'PID': PID}
    return request.make(url, params)

def get_sms_status(request, customerID, userName, userPassword, transactionID, detailed):
    url = SOAP_SERVER + '/HTTP_GetSmsStatus'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
              'transactionID': transactionID, 'detailed': str(detailed).lower()}
    return request.make(url, params)

def inbox(request, customer_id, user_id, password, operation, msg_id):
    url = SOAP_SERVER + '/HTTP_InboxProcessing'
    params = {
        'customerID': customer_id,
        'userName': user_id,
        'userPassword': password,
        'operation':operation,
        'messageId':msg_id
    }
    return request.make(url, params=params)

def inbox_stats(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'stats', None)

def inbox_list_all(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'list-all', None)

def inbox_list_new(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'list-new', None)

def inbox_fetch_all(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'fetch-all', None)

def inbox_fetch_new(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'fetch-new', None)

def inbox_fetch_id(request, customer_id, user_id, password, msg_id):
    return inbox(request, customer_id, user_id, password, 'fetch-id', msg_id)

def inbox_kill_all(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'kill-all', None)

def inbox_kill_old(request, customer_id, user_id, password):
    return inbox(request, customer_id, user_id, password, 'kill-old', None)

def inbox_kill_id(request, customer_id, user_id, password, msg_id):
    return inbox(request, customer_id, user_id, password, 'kill-id', msg_id)

#
# HTTP_Authenticate
#

def test_HTTP_Authenticate_bad_password_fail(request):
    res = authenticate(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['AuthResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['AuthResult']['Originators'] == None
    assert res['AuthResult']['NetPoints'] == '0'
    assert res['AuthResult']['CreditSMS'] == None
    assert res['AuthResult']['CustomerID'] == str(-1)

def test_HTTP_Authenticate_succ(request):
    res = authenticate(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['AuthResult']['Result'] == 'OK'
    assert res['AuthResult']['Originators'] != []
    assert res['AuthResult']['NetPoints'] == 'POSTPAID'
    assert res['AuthResult']['CreditSMS'] == 'POSTPAID'
    assert res['AuthResult']['CustomerID'] == str(CUSTOMER_ID)

def test_HTTP_Authenticate_lower_succ(request):
    res = authenticate(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD.lower())
    assert res['AuthResult']['Result'] == 'OK'
    assert res['AuthResult']['Originators'] != []
    assert res['AuthResult']['NetPoints'] == 'POSTPAID'
    assert res['AuthResult']['CreditSMS'] == 'POSTPAID'
    assert res['AuthResult']['CustomerID'] == str(CUSTOMER_ID)

def test_HTTP_Authenticate_upper_succ(request):
    res = authenticate(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD.upper())
    assert res['AuthResult']['Result'] == 'OK'
    assert res['AuthResult']['Originators'] != []
    assert res['AuthResult']['NetPoints'] == 'POSTPAID'
    assert res['AuthResult']['CreditSMS'] == 'POSTPAID'
    assert res['AuthResult']['CustomerID'] == str(CUSTOMER_ID)

#
# HTTP_KeepAlive
#

def test_HTTP_KeepAlive_bad_password_fail(request):
    res = keep_alive(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['CommonResult']['Result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_KeepAlive_succ(request):
    res = keep_alive(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['CommonResult']['Result'] == 'OK'

#
# HTTP_SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def test_HTTP_SendSms_bad_password_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_originator_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator='', smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_no_recipient_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone='', messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == '600.4 Phone not specified'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_recipient_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=BAD_RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_empty_body_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == 'Message Content Is Empty'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_bad_defdate_fail(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='2014-07-21', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendSms_succ(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == 'POSTPAID'
    assert res['SendResult']['TransactionID'] != None

#
# HTTP_SendBinarySms
#

def test_HTTP_SendBinarySms_bad_password_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_originator_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator='', binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == '600.1 Originator for customerID is not found'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_no_recipient_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone='', defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == '600.4 Phone not specified'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_recipient_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=BAD_RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == 'FAILURE: All recipient numbers in your message are either Rejected or Blacklisted'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_empty_body_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == 'Message Content Is Empty'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_bad_defdate_fail(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='2014-07-21', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == 'Def Date format is incorrect. Correct format is YYYYMMDDHHMMSS'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == '0'
    assert res['SendResult']['TransactionID'] == None

def test_HTTP_SendBinarySms_succ(request):
    res = send_binary_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, binaryBody='7465737420746573742074657374207465737420', recipientPhone=RECIPIENT, defDate='', data_coding='4', esm_class='', PID='')
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['RejectedNumbers'] == None
    assert res['SendResult']['NetPoints'] == 'POSTPAID'
    assert res['SendResult']['TransactionID'] != None

#
# HTTP_GetSmsStatus
#

def test_HTTP_GetSmsStatus_bad_password_fail(request):
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, transactionID='', detailed=False)
    assert res['SmsStatus']['Result'] == '404.2 FAILURE (User is unknown)'
    assert res['SmsStatus']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_empty_transaction_id_fail(request):
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID='', detailed=False)
    assert res['SmsStatus']['Result'] == "605.7 The action you requested cannot be performed, because one of your the required request parameters ('TransactionID') was not supplied."
    assert res['SmsStatus']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_wrong_transaction_id_fail(request):
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=BAD_TRANSACTION_ID, detailed=False)
    assert res['SmsStatus']['Result'] == 'SMS ID for status request is incorrect or not specified'
    assert res['SmsStatus']['NetPoints'] == '0'

def test_HTTP_GetSmsStatus_detailed_false_succ(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['SendResult']['TransactionID']
    time.sleep(2)
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=trans_id, detailed=False)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['NetPoints'] == 'POSTPAID'
    assert res['SmsStatus']['Statistics']['statistics']['SMSC_DELIVERED']['#text'] == "1"

def test_HTTP_GetSmsStatus_detailed_true_succ(request):
    res = send_sms(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, originator=ORIGINATOR, smsText='Hello', recipientPhone=RECIPIENT, messageType='Latin', defDate='', blink=False, flash=False, Private=False)
    trans_id = res['SendResult']['TransactionID']
    time.sleep(2)
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=trans_id, detailed=True)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['NetPoints'] == 'POSTPAID'
    assert res['SmsStatus']['Statistics']['statistics']['SMSC_DELIVERED']['#text'] == "1"
    ## [{'StatusU': u'00440065006c006900760065007200650064'}, {'number': u'375296543210'}]
    StatusU = str(res['SmsStatus']['Details']['details']['SMSC_DELIVERED']['StatusU'])
    assert from_hex_utf16be(StatusU) == 'delivered'
    assert res['SmsStatus']['Details']['details']['SMSC_DELIVERED']['number'] == '375296543210'

#
# HTTP_InboxProcessing
#

def test_HTTP_InboxProcessing_bad_operation_fail(request):
    res = inbox(request, CUSTOMER_ID, USER_ID, PASSWORD, 'bad-operation', None)
    info = res['inbox']
    assert info['result'] == 'Non-supported Inbox operation is specified!'

def test_HTTP_InboxProcessing_stats_bad_password_fail(request):
    res = inbox_stats(request, CUSTOMER_ID, USER_ID, BAD_PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_list_all_bad_password_fail(request):
    res = inbox_list_all(request, CUSTOMER_ID, USER_ID, BAD_PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_kill_old_bad_password_fail(request):
    res = inbox_kill_old(request, CUSTOMER_ID, USER_ID, BAD_PASSWORD)
    idel = res['inboxdel']
    assert idel['result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_InboxProcessing_stats_inbox_not_activated_fail(request):
    res = inbox_stats(request, CUSTOMER_ID, USER_ID_NO_INBOX, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['result'] == 'Inbox is not activated'

def test_HTTP_InboxProcessing_list_all_inbox_not_activated_fail(request):
    res = inbox_list_all(request, CUSTOMER_ID, USER_ID_NO_INBOX, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'Inbox is not activated'

def test_HTTP_InboxProcessing_kill_old_inbox_not_activated_fail(request):
    res = inbox_kill_old(request, CUSTOMER_ID, USER_ID_NO_INBOX, PASSWORD)
    idel = res['inboxdel']
    assert idel['result'] == 'Inbox is not activated'

def test_HTTP_InboxProcessing_stats_empty_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['new'] == '0'
    assert iinfo['total'] == '0'

def test_HTTP_InboxProcessing_stats_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, "Msg")
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['result'] == 'OK'
    assert iinfo['credits'] == 'POSTPAID'
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_list_all_empty_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_list_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_list_all_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_list_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_list_new_empty_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_list_new_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert msg['textU'] == None

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_fetch_all_empty_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_fetch_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_fetch_all_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_fetch_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_fetch_new_empty_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_fetch_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

def test_HTTP_InboxProcessing_fetch_new_succ(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_fetch_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_fetch_id(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    msg_id = msg['@id']

    res = inbox_fetch_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id)
    ilist = res['inboxlist']
    assert ilist['result'] == 'OK'
    assert ilist['credits'] == 'POSTPAID'

    msg = ilist['message']
    assert msg['@new'] == '1'
    assert msg['from'] == RECIPIENT
    assert msg['to'] == SHORT_CODE
    assert msg['msgtype'] == 'SMS'
    assert msg['size'] == str(len(body))
    assert from_hex_utf16be(msg['textU']) == body

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_kill_all(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    info = res['inboxinfo']
    assert info['total'] == '1'
    assert info['new'] == '1'

    res = inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
    idel = res['inboxdel']
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_kill_id(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    msg_id = msg[0]['@id']

    res = inbox_kill_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id)
    idel = res['inboxdel']
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    info = res['inboxinfo']
    assert info['total'] == '1'
    assert info['new'] == '1'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_kill_old(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    msg_id = msg[0]['@id']

    inbox_fetch_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id)

    res = inbox_kill_old(request, CUSTOMER_ID, USER_ID, PASSWORD)
    idel = res['inboxdel']
    assert idel['result'] == 'OK'
    assert idel['deleted'] == '1'

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_check_fetch_and_statuses(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '3'

    # doesn't change read statuses
    inbox_list_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '3'

    # doesn't change read statuses
    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    msg_id = msg[0]['@id']

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '3'

    # changes read status
    inbox_fetch_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '2'

    # changes read statuses
    inbox_fetch_new(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '0'

    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '4'
    assert iinfo['new'] == '1'

    # changes read statuses
    inbox_fetch_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '4'
    assert iinfo['new'] == '0'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

def test_HTTP_InboxProcessing_check_kill_and_statuses(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

    body = "Msg"
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, body)
    time.sleep(1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '3'

    # doesn't change read statuses
    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    msg_id1 = msg[0]['@id']
    msg_id2 = msg[1]['@id']

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '3'
    assert iinfo['new'] == '3'

    inbox_kill_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id1)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '2'

    # changes read status
    inbox_fetch_id(request, CUSTOMER_ID, USER_ID, PASSWORD, msg_id2)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '2'
    assert iinfo['new'] == '1'

    inbox_kill_old(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '1'
    assert iinfo['new'] == '1'

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    res = inbox_stats(request, CUSTOMER_ID, USER_ID, PASSWORD)
    iinfo = res['inboxinfo']
    assert iinfo['total'] == '0'
    assert iinfo['new'] == '0'

def test_HTTP_InboxProcessing_check_timestamp_is_local_time(request):
    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)

    send_inbound_via_smppsim(RECIPIENT, SHORT_CODE, "Msg")
    time.sleep(1)

    res = inbox_list_new(request, CUSTOMER_ID, USER_ID, PASSWORD)
    msg = res['inboxlist']['message']
    timestamp = msg['timestamp']

    received = datetime.datetime.strptime(timestamp, '%Y%m%d%H%M%S')
    now = datetime.datetime.now()
    assert (now - received) < datetime.timedelta(minutes=1)

    inbox_kill_all(request, CUSTOMER_ID, USER_ID, PASSWORD)
