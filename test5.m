% test every token, doing some random computations

begin
	y := 0;
	x := -1+((2-(3*4))/5)+6;
	write x;
	if (x) then 
		while x
			do
			begin
				x:=x-1;
				y := y+2;
			end
	else
		if (x-1)
			then x := 0
			else y := 1;
	write (y);
	input x;
	write (x*y);
end