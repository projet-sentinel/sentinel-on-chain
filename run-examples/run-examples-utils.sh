#!/bin/bash

export GREEN=$(tput setaf 2)
export BLUE=$(tput setaf 4)
export CYAN=$(tput setaf 6)
export NORMAL=$(tput sgr0)

balanceUser() {
    printf "\n${GREEN}$1${NORMAL}\n"
    ./balance-wallet.sh $1
}

balanceValidator() {
    printf "\n${GREEN}$1${NORMAL}\n"
    ./balance-validator.sh $1
}

printfGreen() {
    printf "\n${GREEN}$1${NORMAL}\n"
}

printfCyan() {
    printf "\n${CYAN}$1${NORMAL}\n"
}

sleep2() {
    sleep 5
}

pressContinue() {
    read -p "${BLUE}Press Enter to continue...${NORMAL}"
}