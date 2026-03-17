**free

dcl-f hellod workstn;

dow not *in03;
  exfmt screen;
  if *in03;
    leave;
  endif;
  if name <> *blanks;
    message = 'Hello, ' + %trim(name) + '! Welcome to IBM i.';
  else;
    message = *blanks;
  endif;
enddo;

*inlr = *on;
