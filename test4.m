begin
	y := 100;
	x := 100;
	while x do
		begin 
			x := x-1;
			while y do
				begin
					y := y-1;
					write y;
				end;
			write x;
		end;
	write (x*y);
end

