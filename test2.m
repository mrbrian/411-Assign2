% error handling

begin
	x:=100 %; %  error  no semicolon
	y:=1; 
	if (x) then y:=10; % else y := 100;      % error, no else
	while (x)   %do begin x:=x-1; y:=y+1; end;     %error no do
	write y;
end
randomToken := -1   % handle error when there are leftover tokens