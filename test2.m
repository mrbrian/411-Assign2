/* This program calculates the
    factorial of the number which
    is input */

begin
    input x;                    % input a number .. 
    z := 1;
    y := 1;
    while (x*2) do
        begin
            y :=  y * x;
            x :=  x - 1;
         end;
    write y;
end