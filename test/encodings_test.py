# -*- coding: utf-8 -*-

# setup python's virtualenv as described here
# https://gist.github.com/ten0s/98e7d88476ec75351d75

# $ . env/bin/activate
# $ py.test soap_test.py
# $ py.test soap_test.py -k test_encodings
# $ py.test --pdb
# $ py.test -v

# make standalone test script and then run it in verbose mode
# $ py.test --genscript=runtests.py
# $ python runtests.py -v

import pytest
import requests
import xmltodict
import hexdump

HOST = 'http://localhost:8088/bmsgw/soap/messenger.asmx'
#HOST = 'http://mm.powermemobile.com/mm/soap/messenger.asmx'

CUSTOMER_ID = 3
USER_ID     = 'user'
PASSWORD    = 'password'

ORIGINATOR = 'SMS'
RECIPIENT = '375296543210'

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
    url = HOST + '/HTTP_SendSms'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'originator': originator, 'smsText': smsText, 'recipientPhone': recipientPhone, 'messageType': messageType, \
    'defDate': defDate, 'blink': str(blink), 'flash': str(flash), 'Private': str(Private)}
    return request.make(url, params)

def get_sms_status(request, customerID, userName, userPassword, transactionID, detailed):
    url = HOST + '/HTTP_GetSmsStatus'
    params = {'customerID': str(customerID), 'userName': userName, 'userPassword': userPassword, \
    'transactionID': transactionID, 'detailed': str(detailed)}
    return request.make(url, params)


#
# Check encodings
#

# messageType=Latin|ArabicWithArabicNumbers|ArabicWithLatinNumbers

def check_message_parts_count(request, message, encoding, count):
    res = send_sms(request, CUSTOMER_ID, USER_ID, PASSWORD, ORIGINATOR, message, RECIPIENT, encoding, '', False, False, False)
    assert res['SendResult']['Result'] == 'OK'
    assert res['SendResult']['TransactionID'] != None
    tid = res['SendResult']['TransactionID']

    res = get_sms_status(request, CUSTOMER_ID, USER_ID, PASSWORD, tid, False)
    assert res['SmsStatus']['Result'] == 'OK'
    stats = res['SmsStatus']['Statistics']['statistics']
    # w/o namespace value, which is first
    stats2 = stats.values()[1:]
    assert sum(map(lambda d: int(d['#text']), stats2)) == count

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
    checks = [
        (latin1_160, 'Latin', 1),
        (latin1_161, 'Latin', 2),
        (latin1_306, 'Latin', 2),
        (latin1_307, 'Latin', 3),
        (latin1_459, 'Latin', 3),
        (latin1_460, 'Latin', 4),
        (utf8_70,    'Latin', 1),
        (utf8_71,    'Latin', 2),
        (utf8_134,   'Latin', 2),
        (utf8_135,   'Latin', 3),
        (utf8_201,   'Latin', 3),
        (utf8_202,   'Latin', 4)
    ]

    for (message, encoding, count) in checks:
        check_message_parts_count(request, message, encoding, count)
