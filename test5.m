% try everything, just doing random stuff

begin
	y := 0;
	x := -1+(2-(3*4)/5)+6;
	if (x) then 
		while x
			do
			begin
				x:=x-1;
				y := y+1;
			end
	else
		if (x-1)
			then x := 0
			else y := 1;
	input x;
	write y;
end