# lists to hold the arguments
search_args=()
exclude_args=()

# last seen character
seen_c="i"
file="unknown.log"

# loop through all arguments passed to the script
for arg in "$@"; do
    # check for options
    if [ "${arg:0:1}" == "-" ]; then
        seen_c="${arg:1:1}"
        continue
    fi

    # add argument to list based on option selected
    if [ "$seen_c" == "v" ]; then
        exclude_args+=("$arg")
    elif [ "$seen_c" == "i" ]; then
        search_args+=("$arg")
    elif [ "$seen_c" == "f" ]; then
        file="$arg.log"
    elif [ "$seen_c" == "h" ]; then
        echo "Usage: $0 <search_term1> <search_term2> ... -v <exclude_term1> <exclude_term2> ..."
        exit 0
    else
        echo "Error: Unknown option -$seen_c"
        exit 1
    fi
done

if [ -f "$file" ]; then
    rm "$file"
fi

# ensure at least one search argument was provided
if [ ${#search_args[@]} -eq 0 ]; then
    echo "Error: No search terms provided before -v"
    exit 1
fi

# build command
cmd="grep -rn -I --exclude='*.sh' ${search_args[0]}"


# NEEED TO ONLY GET ARGS AFTER FIRST ONE
for s_arg in "${search_args[@]}"; do
    cmd+=" $(printf '%q' "$s_arg")"
done

# append exclusions
for e_arg in "${exclude_args[@]}"; do
    cmd+=" | grep -v $(printf '%q' "$e_arg")"
done

# append last part of command
cmd+=" | sed 's|^|/home/anderslt/gibbon-compiler/|g' | sponge $(printf '%q' "$file")"

# execute
eval "$cmd"
