#!/bin/bash
# we are not multi-region yet
set -x AWS_DEFAULT_REGION us-east-1
 
# account IDs (copied from the AWS Okta tile sign in, if you have more/others)
set -x PCYCLE 106877218800
set -x PPROD 386675210126
set -x PSTAGE 486598304777
set -x PTEST 152245890419
set -x TKITCH 048438595429
set -x PES1 429007243955
 
# prefer to get push notifications (switch to Passcode if you like)
set -x FORCE_MFA "Duo Push"
 
# login to an account if necessary
function s2al
	 saml2aws login --skip-prompt --profile=$argv[1] --duo-mfa-option "$FORCE_MFA" --role="arn:aws:iam::$argv[2]:role/data-engineer";
end

# inject the active credentials for an account into your env
function s2a
	 saml2aws script --shell=fish --skip-prompt --profile=$argv[1] | source
end

# shortcut to remember who you are (and which account you are in)
function awho
	 aws sts get-caller-identity;
end
 
# these are the aliases to trigger login (if necessary)
alias apc="s2al pc $PCYCLE"
alias atest="s2al test $PTEST"
alias astage="s2al stage $PSTAGE"
alias aprod="s2al prod $PPROD"
 
# these are the aliases to trigger account switch
alias spc="s2a pc"
alias stest="s2a test"
alias sstage="s2a stage"
alias sprod="s2a prod"
 
# these are the aliases to trigger account login (if necessary) and switch
alias jpc="s2al pc $PCYCLE && s2a pc"
alias jtest="s2al test $PTEST && s2a test"
alias jstage="s2al stage $PSTAGE && s2a stage"
alias jprod="s2al prod $PPROD && s2a prod"