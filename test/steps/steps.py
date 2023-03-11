from behave import *

import subprocess
import os

@given('flp22-fun is available')
def step_impl(context):
  rc = subprocess.call(
    'cd ..; make',
    shell=True,
    stdout=subprocess.DEVNULL,
  )
  assert rc == 0

@when('flp22-fun is run with no options')
def step_impl(context):
  p = os.popen(f'../flp22-fun')
  context.out = p.read().rstrip()
  context.status = p.close()

@when('flp22-fun is run with "{option}" option and "{file}" file')
def step_impl(context, option, file):
  p = os.popen(f'../flp22-fun {option} {file}')
  context.out = p.read().rstrip()
  context.status = p.close()

@when('flp22-fin is run with "{option}" option and "{file}" as stdin')
def step_impl(context, option, file):
  p = os.popen(f'cat {file} | ../flp22-fun {option}')
  context.out = p.read().rstrip()
  context.status = p.close()

@then('output matches file "{file}"')
def step_impl(context, file):
  with open(file, 'r') as f:
    expected = f.read().rstrip()

  assert context.out == expected

@then('return code is "{rc}"')
def step_impl(context, rc):
  if context.status:
    assert os.waitstatus_to_exitcode(context.status) == int(rc)
  else:
    assert 0 == int(rc)
