results_folder="output_results"
home_dir=$(realpath "$(pwd)/../../../..")
l2_parser_native_dir="$home_dir/gibbon-compiler/src/Gibbon/L2ParserNative"
input_l2_files=$(ls $l2_parser_native_dir/tests/*.hs)
stripped_input_l2_files=$(ls $l2_parser_native_dir/tests/*.hs | xargs -n 1 basename -s .hs)

mkdir -p $results_folder

corr_file_nums=()
extra_parameters=()
while [ $# -gt 0 ]; do
    key="$1"
    case $key in
        all)
            corr_file_nums=()
            shift 1;;
        [0-9]*)
            corr_file_nums+=("$1")
            shift 1;;
        -*)
            extra_parameters+=("$1")
            shift 1;;
        *)
            echo "Unknown option: $key"
            exit 1;;
    esac
done
# if no file numbers are provided, run all tests
if [ ${#corr_file_nums[@]} -eq 0 ]; then
    corr_files=$stripped_input_l2_files
    echo "Running all tests."
else
    corr_files=()
    for file in $stripped_input_l2_files; do    
        first_number=$(echo $file | grep -oE '^[0-9]+')
        for file_number in "${corr_file_nums[@]}"; do
            if [ "$first_number" == "$file_number" ]; then
                corr_files+=($file)
            fi
        done
    done
fi

cd $home_dir && source set_env.sh
cd gibbon-compiler
echo "Compiling Gibbon..."
cabal v2-build
for file in $corr_files; do
    output_file=$l2_parser_native_dir/$results_folder/${file}_output.txt
    echo ""
    echo "Running test for file: $file.hs with extra parameters: $extra_parameters"
    echo "Output will be saved to: $output_file"
    echo "Ouput from L2 Parser: " > $output_file
    cabal v2-run gibbon -v0 -- --run --l2 --packed --no-ran $extra_parameters $l2_parser_native_dir/tests/$file.hs >> $output_file
    echo "" >> $output_file
    echo "Output from L2 Parser Native: " >> $output_file
    cabal v2-run gibbon -v0 -- --run --packed --no-ran $extra_parameters $l2_parser_native_dir/tests/gibbon-native/$file.hs >> $output_file
    rm $l2_parser_native_dir/tests/$file.c
    rm $l2_parser_native_dir/tests/$file.exe
    rm $l2_parser_native_dir/tests/gibbon-native/$file.c
    rm $l2_parser_native_dir/tests/gibbon-native/$file.exe
done
