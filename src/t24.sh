#!/bin/bash

swipl -s agentspeak-l.pl -g "consult('parser.pl'),start_agent,load_agent('t/t24.pasl'),solve(!test),halt."
