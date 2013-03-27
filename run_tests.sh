for LESS in `find tests -name '*.less'`
do
    BASE="${LESS%.less}"
    CORRECT_CSS="$BASE.css"
    OUTPUT_CSS="$BASE.out.css"
    echo $BASE
    ./Less < $LESS > $OUTPUT_CSS
    diff -q $CORRECT_CSS $OUTPUT_CSS
done
