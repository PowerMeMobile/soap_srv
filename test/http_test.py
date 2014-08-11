# setup python's virtualenv as described here
# https://gist.github.com/ten0s/98e7d88476ec75351d75

# $ . env/bin/activate
# $ py.test http_test.py
# $ py.test http_test.py -k test_Authenticate_bad_user_fail
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest
import requests
import xmltodict

HOST = 'http://localhost:8088/bmsgw/soap/messenger.asmx'
#HOST = 'http://mm.powermemobile.com/mm/soap/messenger.asmx'

CUSTOMER_ID = 3
USER_ID     = 'user'
PASSWORD    = 'password'
BAD_PASSWORD = 'intentionally wrong password'

ORIGINATOR = 'SMS'
RECIPIENT = '375293615363'
BAD_RECIPIENT = '999999999999'
RECIPIENT_BASE64 = 'Mzc1Mjk2NTQzMjEw'
BAD_RECIPIENT_BASE64 = 'OTk5OTk5OTk5OTk5'

TRANSACTION_ID = '85ccccbf-f854-4898-86b1-5072d3e33da1'
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
        elif self.method == 'POST':
            req = requests.post(url, data = params)
        return xmltodict.parse(req.text)

@pytest.fixture(scope="function",
                params=["GET", "POST"])
def request(request):
    return HttpRequest(request.param)

#
# Utils
#

def authenticate(request, customerID, userName, userPassword):
    url = HOST + '/HTTP_Authenticate'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword}
    return request.make(url, params)

def keep_alive(request, customerID, userName, userPassword):
    url = HOST + '/HTTP_KeepAlive'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword}
    return request.make(url, params)

def send_sms(request, customerID, userName, userPassword, originator, smsText, recipientPhone, messageType, defDate, blink, flash, Private):
    url = HOST + '/HTTP_SendSms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'originator': originator, 'smsText': smsText, 'recipientPhone': recipientPhone, 'messageType': messageType, \
    'defDate': defDate, 'blink': str(blink), 'flash': str(flash), 'Private': str(Private)}
    return request.make(url, params)

def send_binary_sms(request, customerID, userName, userPassword, originator, binaryBody, recipientPhone, defDate, data_coding, esm_class, PID):
    url = HOST + '/HTTP_SendBinarySms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'originator': originator, 'binaryBody': binaryBody, 'recipientPhone': recipientPhone, 'defDate': defDate, \
    'data_coding': data_coding, 'esm_class': esm_class, 'PID': PID}
    return request.make(url, params)

def get_sms_status(request, customerID, userName, userPassword, transactionID, detailed):
    url = HOST + '/HTTP_GetSmsStatus'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'transactionID': transactionID, 'detailed': str(detailed)}
    return request.make(url, params)

#
# HTTP_Authenticate
#

def test_HTTP_Authenticate_bad_user_fail(request):
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

#
# HTTP_KeepAlive
#

def test_HTTP_KeepAlive_bad_user_fail(request):
    res = keep_alive(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD)
    assert res['CommonResult']['Result'] == '404.2 FAILURE (User is unknown)'

def test_HTTP_KeepAlive_succ(request):
    res = keep_alive(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD)
    assert res['CommonResult']['Result'] == 'OK'

#
# HTTP_SendSms
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def test_HTTP_SendSms_bad_user_fail(request):
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

def test_HTTP_SendBinarySms_bad_user_fail(request):
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

def test_HTTP_GetSmsStatus_bad_user_fail(request):
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=BAD_PASSWORD, transactionID=TRANSACTION_ID, detailed=False)
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
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=TRANSACTION_ID, detailed=False)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['NetPoints'] == 'POSTPAID'
    assert res['SmsStatus']['Statistics']['statistics']['SMSC_DELIVERED']['#text'] == "1"

def test_HTTP_GetSmsStatus_detailed_true_succ(request):
    res = get_sms_status(request, customerID=CUSTOMER_ID, userName=USER_ID, userPassword=PASSWORD, transactionID=TRANSACTION_ID, detailed=True)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['NetPoints'] == 'POSTPAID'
    assert res['SmsStatus']['Statistics']['statistics']['SMSC_DELIVERED']['#text'] == "1"
    ## [{'StatusU': u'00440065006c006900760065007200650064'}, {'number': u'375296543210'}]
    assert res['SmsStatus']['Details']['details']['SMSC_DELIVERED']['StatusU'] == '00440065006c006900760065007200650064' # 'delivered'
    assert res['SmsStatus']['Details']['details']['SMSC_DELIVERED']['number'] == '375296543210'
