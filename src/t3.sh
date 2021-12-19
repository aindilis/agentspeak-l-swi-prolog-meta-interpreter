#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t3'),start_agent,solve(!test3),halt."
