export USER_DIR=$(pwd)/

# https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel
SOURCE="${BASH_SOURCE[0]:-$0}";
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )";
  SOURCE="$( readlink -- "$SOURCE"; )";
  [[ $SOURCE != /* ]] && SOURCE="${DIR}/${SOURCE}"; # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname -- "$SOURCE"; )" &> /dev/null && pwd 2> /dev/null; )";

cd $DIR



if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # LD_LIBRARY_PATH=./ ./color-picker-bin
    echo "NO NEED TO CALL THIS FILE"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    DYLD_FALLBACK_LIBRARY_PATH=./ ./color-picker-bin
else
    echo "Unknown Platform."
fi

