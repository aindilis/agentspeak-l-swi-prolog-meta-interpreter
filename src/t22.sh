#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('parser.pl'),start_agent,load_agent('t/t22.pasl'),solve(!evaluate_salary),halt."
