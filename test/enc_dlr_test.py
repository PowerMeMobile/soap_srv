# -*- coding: utf-8 -*-

import pytest

import os
import requests
import xmltodict
import hexdump
import time as time
import datetime

SOAP_HOST = os.getenv('SOAP_HOST')
if SOAP_HOST == None or SOAP_HOST == '':
    SOAP_HOST = '127.0.0.1'

SOAP_PORT = os.getenv('SOAP_PORT')
if SOAP_PORT == None or SOAP_PORT == '':
    SOAP_PORT = '8088'

KELLY_HOST = os.getenv('KELLY_HOST')
if KELLY_HOST == None or KELLY_HOST == '':
    KELLY_HOST = SOAP_HOST

KELLY_PORT = os.getenv('KELLY_PORT')
if KELLY_PORT == None or KELLY_PORT == '':
    KELLY_PORT = '8080'

SOAP_SERVER = 'http://{0}:{1}/bmsgw/soap/messenger.asmx'.format(SOAP_HOST, SOAP_PORT)
KELLY_SERVER = 'http://{0}:{1}'.format(KELLY_HOST, KELLY_PORT)

CUSTOMER_ID = 10003
USER_ID     = 'user'
PASSWORD    = 'password'

ORIGINATOR = '375296660005'
SIM_RECIPIENT = '375296543210'
SINK_RECIPIENT = '999296543210'

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

@pytest.fixture(scope="function", params=["GET", "POST"])
def request(request):
    return HttpRequest(request.param)

#
# Utils
#

def send_sms(request, customerID, userName, userPassword, originator, smsText, recipientPhone, messageType, defDate, blink, flash, Private):
    url = SOAP_SERVER + '/HTTP_SendSms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'originator': originator, 'smsText': smsText, 'recipientPhone': recipientPhone, 'messageType': messageType, \
    'defDate': defDate, 'blink': str(blink), 'flash': str(flash), 'Private': str(Private)}
    return request.make(url, params)

def get_sms_status(request, customerID, userName, userPassword, transactionID, detailed):
    url = SOAP_SERVER + '/HTTP_GetSmsStatus'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'transactionID': transactionID, 'detailed': str(detailed)}
    return request.make(url, params)

def get_batch_info(_request, transactionID):
    url = KELLY_SERVER + '/v1/batches/' + transactionID
    req = requests.get(url)
    print("{0}".format(req.request.url))
    return req.json()

#
# Check encodings
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def check_message_parts_count(request, message, encoding, count):
    res = send_sms(request, CUSTOMER_ID, USER_ID, PASSWORD, ORIGINATOR, message, SIM_RECIPIENT, encoding, '', False, False, False)
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['TransactionID'] != None
    tid = res['SendResult']['TransactionID']

    time.sleep(1)

    res = get_batch_info(request, tid)
    assert res['messages'] == count

def test_check_encodings(request):
    latin1_160 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJ'
    latin1_161 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJK'
    latin1_306 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345'
    latin1_307 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456'
    latin1_459 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxy'
    latin1_460 = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
    utf8_70 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123'
    utf8_71 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ01234'
    utf8_134 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧ'
    utf8_135 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШ'
    utf8_201 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНО'
    utf8_202 = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ0123456789абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОП'
    latin1_checks = [
        (latin1_160, 'Latin', 1),
        (latin1_161, 'Latin', 2),
        (latin1_306, 'Latin', 2),
        (latin1_307, 'Latin', 3),
        (latin1_459, 'Latin', 3),
        (latin1_460, 'Latin', 4)
    ]
    utf8_checks = [
        (utf8_70,    'ArabicWithLatinNumbers', 1),
        (utf8_71,    'ArabicWithLatinNumbers', 2),
        (utf8_134,   'ArabicWithLatinNumbers', 2),
        (utf8_135,   'ArabicWithLatinNumbers', 3),
        (utf8_201,   'ArabicWithLatinNumbers', 3),
        (utf8_202,   'ArabicWithLatinNumbers', 4)
    ]
    for (message, encoding, count) in latin1_checks:
        check_message_parts_count(request, message, encoding, count)
    for (message, encoding, count) in utf8_checks:
        check_message_parts_count(request, message, encoding, count)

#
# Check delivery statuses
#
# You need smppsink (https://github.com/PowerMeMobile/smppsink) to run these tests
#

def check_sink_delivery_status(request, command, status, timeout):
    res = send_sms(request, CUSTOMER_ID, USER_ID, PASSWORD, ORIGINATOR, command, SINK_RECIPIENT, 'Latin', '', False, False, False)
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['TransactionID'] != None
    tid = res['SendResult']['TransactionID']

    time.sleep(timeout)

    res = get_sms_status(request, CUSTOMER_ID, USER_ID, PASSWORD, tid, False)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['Statistics']['statistics'][status]


def test_check_sink_delivery_statuses(request):
    checks = [
        ('receipt:enroute',       'SMSC_ENROUTE',       3),
        ('receipt:delivered',     'SMSC_DELIVERED',     3),
        ('receipt:expired',       'SMSC_EXPIRED',       3),
        ('receipt:deleted',       'SMSC_DELETED',       3),
        ('receipt:undeliverable', 'SMSC_UNDELIVERABLE', 3),
        ('receipt:accepted',      'SMSC_ACCEPTED',      3),
        ('receipt:unknown',       'SMSC_UNKNOWN',       3),
        ('receipt:rejected',      'SMSC_REJECTED',      3),
        # ('submit:{timeout:43200}','SMSC_???', 1), # in 12 hrs # smppsink is not ready for this yet
        ('submit:1',              'SMSC_FAILED',        3)
    ]

    for (command, status, timeout) in checks:
        check_sink_delivery_status(request, command, status, timeout)

#
# Check deferred status
#

def test_check_deferred_status(request):
    defdate = (datetime.datetime.now() + datetime.timedelta(days=1)).strftime('%Y%m%d%H%M%S')
    res = send_sms(request, CUSTOMER_ID, USER_ID, PASSWORD, ORIGINATOR, 'Hello', SINK_RECIPIENT, 'Latin', defdate, False, False, False)
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['TransactionID'] != None
    tid = res['SendResult']['TransactionID']

    time.sleep(2)

    res = get_sms_status(request, CUSTOMER_ID, USER_ID, PASSWORD, tid, False)
    assert res['SmsStatus']['Result'] == 'OK'
    assert res['SmsStatus']['Statistics']['statistics']['SMSC_DEFERRED']
