% testing nested loops    5 cycles of 5

begin
	x := 5;
	while x do
		begin 
			y := 5;
			while y do
				begin
					write y;
					y := y-1;
				end;			
			x := x-1;
		end;
end

