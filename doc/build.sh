#!/usr/bin/env bash
# Write git commit hash into documentation.

# Bypass interactive questioning if any argument is passed in.
say_yes=""
if [[ $# -gt 0 ]]; then
    say_yes="y"
fi

# Give warning if the git repository is unclean in any way (there are either
# staged or unstaged changes).
if [[ $(git status --porcelain -uno | wc -l) -gt 0 ]]; then
    echo "[WARNING] Some tracked files are modified:"
    git status -s -uno
    if [[ -z $say_yes ]]; then
        while true; do
            read 'reply?Continue anyway? (y/n): '
            case $reply in
                [Yy]) break ;;
                [Nn]) exit 0 ;;
                *) printf '%s\n' 'Please answer y or n.' ;;
            esac
        done
    fi
fi

# Find latest commit hash and date.
commit_hash_date=$(git rev-list --all -1 --pretty="format:%ci")
commit_hash=$(echo $commit_hash_date | head -n1 | cut -d " " -f2)
commit_date=$(echo $commit_hash_date | tail -n1)
commit_desc=$(git describe --long $commit_hash | cut -c2-)

# Replace placeholder text.
cat keylo.tex | sed \
    -e "s/GIT-COMMIT-DESC/$commit_desc/"\
    -e "s/GIT-COMMIT-HASH/$commit_hash/"\
    -e "s/GIT-COMMIT-DATE/$commit_date/"\
    > keylo-versioned.tex

# Remove old versions.
rm -f keylo-*.pdf

# Compile PDF.
make

# Append $commit_desc to manual's filename.
cp -f keylo-versioned.pdf keylo-$commit_desc.pdf
