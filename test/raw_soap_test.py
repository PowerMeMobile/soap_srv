# -*- coding: utf-8 -*-

import sys
reload(sys)
sys.setdefaultencoding('utf-8')

import pytest

import os
import requests
from requests import Request, Session
import xmltodict
import time as time

SOAP_HOST = os.getenv('SOAP_HOST')
if SOAP_HOST == None or SOAP_HOST == '':
    SOAP_HOST = '127.0.0.1'

SOAP_PORT = os.getenv('SOAP_PORT')
if SOAP_PORT == None or SOAP_PORT == '':
    SOAP_PORT = '8088'

SOAP_URL = 'http://{0}:{1}/bmsgw/soap/messenger.asmx'.format(SOAP_HOST, SOAP_PORT)

#
# Utils
#

def send_soap(xml, length):
    headers = {
        'Soapaction': '"http://pmmsoapmessenger.com/SendSms"',
        'Content-Type': 'text/xml; charset="UTF-8"',
    }
    with requests.Session() as s:
        req = Request('POST', SOAP_URL, data=xml, headers=headers)
        prep = req.prepare()
        prep.headers['Content-Length'] = length
        #print prep.headers
        resp = s.send(prep)
        #print resp.text
        return xmltodict.parse(resp.text)

#
# Content-Length tests
#

def test_Correct_Content_Length_succ():
    xml = open('arabic.xml').read()
    length = len(xml)
    assert length == 764
    res = send_soap(xml, length)
    assert res[u'soap:Envelope'][u'soap:Body'][u'HTTP_SendSmsResponse'][u'HTTP_SendSmsResult'][u'Result'] == u'404.2 FAILURE (User is unknown)'

def test_Incorrect_Content_Length_succ():
    xml = open('arabic.xml').read()
    incorrect_length = len(xml) - 15
    res = send_soap(xml, incorrect_length)
    assert res[u'soap:Envelope'][u'soap:Body'][u'soap:Fault'][u'faultstring'] == u'malformed_xml'

def test_Multiple_Incorrect_Content_Length_succ():
    for i in xrange(5000):
        test_Incorrect_Content_Length_succ()
