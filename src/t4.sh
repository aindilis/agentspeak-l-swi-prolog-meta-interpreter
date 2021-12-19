#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t4'),start_agent,solve(!test4),halt."
