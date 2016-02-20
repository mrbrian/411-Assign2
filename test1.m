% testing arithmetic operations and order of operations

begin
	x := 1;
	y := 2;   %  test /*
	a := 3;
	b := 4;
	
	a := (1 + (((x-y) / (a+b)))) * 5;
	b := 1 + ((((x-y) / (a+b))) * 5);
	c := 100 / ((((x-y)*20 / (a+b))) * 5);
	write (1 + 2);
	write (1 - 2);
	write (10 / 2);
	write (10 * 2);
	write a;
	write b;
	write c;
end
% asdf