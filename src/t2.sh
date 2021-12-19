#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t2'),start_agent,solve(!evaluate_salary),halt."
