PROGRAM Sort(input, output);
    CONST
        MaxElts = 5;

    VAR
        W, OP, i, j, tmp, size: integer;

        arr: ARRAY [1..MaxElts] OF Integer;

    FUNCTION Split(start, stop: integer): integer;
        VAR
            left, right: integer;
            pivot: integer;
            t: integer;

        BEGIN
            pivot := arr[start];
            left := start + 1;
            right := stop;

            WHILE left <= right DO BEGIN
                WHILE (left <= stop) AND (arr[left] < pivot) DO
                    left := left + 1;
                WHILE (right > start) AND (arr[right] >= pivot) DO
                    right := right - 1;
                IF left < right THEN
                BEGIN
                    t := arr[left];
                    arr[left] := arr[right];
                    arr[right] := t;
                END;
            END;

            t := arr[start];
            arr[start] := arr[right];
            arr[right] := t;

            Result := right
        END;


    FUNCTION Quicksort(start, stop: integer):integer;
        VAR
            O, splitpt: integer;

        BEGIN  
            IF start < stop THEN BEGIN
                splitpt := Split(start, stop);
                O := Quicksort(start, splitpt-1);
                O := Quicksort(splitpt+1, stop);
            END;
            Result := 0;
        END;


    BEGIN
        arr[1] := 5;
        arr[2] := 4;
        arr[3] := 6;
        arr[4] := 9;
        arr[5] := 3;

        { Sort the contents. }
        OP := Quicksort(1, 5);

        { Print. }
        writeln(arr[1]);
        writeln(arr[2]);
        writeln(arr[3]);
        writeln(arr[4]);
        writeln(arr[5]);
    END.