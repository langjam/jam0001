from http.server import BaseHTTPRequestHandler, HTTPServer

import os
import sys
import threading

from .values import *

class SimpleHttpServer(BaseHTTPRequestHandler):
  pass

def get_httpserve_lib():

  def _create_handler(throw_token, method, path, handler_value):
    return new_error_value(throw_token, "create handler not implemented yet.")

  def _start(throw_token, port):
    return new_error_value(throw_token, "server start not implemented yet")

  return {
    'create_handler': _create_handler,
    'start': _start,
  }
