% loop

begin
	x:=100;
	y:=1; 
	if (x) then y:=10 else y := 100;    
	while (x) 
	do 
		begin 
			x:=x-1; 
			y:=y+1; 
		end;
	write y;
end