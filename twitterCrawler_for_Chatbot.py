#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
# ----------------------------------------------------------------------------
# -- Twitter Get All Conversations
# -- Created by Kerem Vatandas / Hincal Topcuoglu - keremvatandas@gmail.com / hincal.topcuoglu@gmail.com
# ----------------------------------------------------------------------------
'''

import os
os.chdir("C:\\Users\\Administrator\\Desktop\\twitter\\chatbot\\crawler\\")


import tweepy
import configparser
import time
# GLOBAL PATH
CONF_FILE = "config.ini"
ACCOUNTS = "accounts.txt"

CONFIG = configparser.ConfigParser()
CONFIG.read(CONF_FILE)

CONSUMER_KEY = CONFIG.get('KEYS', 'consumer_keys')
CONSUMER_SECRET = CONFIG.get('KEYS', 'consumer_secret')
ACCESS_TOKEN = CONFIG.get('KEYS', 'access_token')
ACCESS_TOKEN_SECRET = CONFIG.get('KEYS', 'access_token_secret')


auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_TOKEN, ACCESS_TOKEN_SECRET)
api = tweepy.API(auth)


def get_user_id(user_name):
    """ Get User Id
        parameters:
            user_name: Str
        return:
            user_id: Int
        example:
            get_user_id("keremvatandas")
            >> 1460607360
    """
    return api.get_user(screen_name=user_name).id


def write_to_file(page_list):
    """ Write to File"""
    _test = list()
    with open('konusmaDatasi.txt', 'a+') as _f:
        print("Dosya yazmaya basladi...")
        for conv in page_list:

            try:
                _first = api.get_status(
                    conv.id_str, tweet_mode='extended')._json['full_text']
                print(_first.strip())
            except Exception:
                _first = None
                continue

            try:
                _second = api.get_status(
                    conv.in_reply_to_status_id_str,
                    tweet_mode='extended')._json['full_text']
                print(_second.strip())
            except Exception:
                _second = None
                continue
            _test.append("{1} ++ {0}\n\n".format(_first.strip(),
                                                 _second.strip()))

        _f.writelines(_test)


def get_timeline(user_id):
    """ Get Conversations
        parameters:
            user_id: Int
        return:
            None
        example:
            get_timeline(1460607360)
    """
    try:
        _tmp = list()

        for page in tweepy.Cursor(api.user_timeline, id=user_id).items():
            _tmp.append(page)
        print("Page toplandi...")
        write_to_file(_tmp)

    except Exception as err:
        print(err)  # Logger gelecek


# tweepy.api.get_status(haha.in_reply_to_status_id_str).text
if __name__ == "__main__":
    with open(ACCOUNTS,encoding="utf-8") as _f:
        accounts = _f.readlines()
    for account in accounts:
        print(account)
        get_timeline(get_user_id(account.encode('utf-8').strip()))
    print("\n\n\n ##### TUM KONUSLAR BASARIYLA TAMAMLANDI ##### \n\n\n")
