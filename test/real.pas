PROGRAM RealTest(input, output);
    VAR
        Step, flag: integer;
        A, ans: real;

    BEGIN
        read(Step);
        flag := 1;
        A := 1.0;
        REPEAT
            If flag mod 2 = 1 then ans := ans + 1.0 / A
            else ans := ans - 1.0 / A;
            flag := flag + 1;
            A := A + 2.0;
            Step := Step - 1;
        UNTIL Step = 0; {pi / 4 = 1 - 1/3 + 1/5 - 1/7 ...}
        ans := ans * 4.0;
        writeln(ans); 
    END.