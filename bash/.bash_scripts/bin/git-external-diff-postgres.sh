if ! [ -x "$(command -v colordiff)" ]; then
	diff --label a/"$1" --label b/"$1" $DIFF_OPTS "$2" "$5"
else
	diff --label a/"$1" --label b/"$1" $DIFF_OPTS "$2" "$5" | colordiff
fi