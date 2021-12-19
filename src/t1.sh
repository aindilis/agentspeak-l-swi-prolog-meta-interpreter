#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t1'),start_agent,solve(!test1),halt."
