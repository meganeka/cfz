function cfz_history () {
    cmd=$(cfz_wrapper < "$HOME/.bash_history")
    echo "$cmd"
}

# bind '"\C-r":"\C-A\C-K$(cfz_history) \e\C-e"'
