#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t20'),start_agent,solve(!isolate),halt."
