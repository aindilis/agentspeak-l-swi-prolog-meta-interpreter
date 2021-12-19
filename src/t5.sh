#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t5'),start_agent,solve(!test5),wait(100),halt"
