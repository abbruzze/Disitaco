program testFile;
uses crt;
Const
  TXDATA = $2F8;
  LCR = $2FB;
  MODEM_CONTROL = $2FC;
  MODEM_STATUS = $2FE;
  LSR = $2FD;
  COM1_OFFSET = $100;

  STATUS_OK = 0;
  STATUS_CMD_NOT_FOUND = 1;
  STATUS_LOCALDIR_NOT_FOUND = 2;
Var
   comOffset:Integer;

procedure setupSerial;
begin
  port[LCR + comOffset] := port[LCR + comOffset] or $80;
  port[TXDATA + comOffset] := $06; {19200 seems to work fine}
  port[TXDATA + comOffset + 1] := $00;
  port[LCR + comOffset] := 3; {8 data bit}
end;

procedure setTurbo_ON;
begin
  port[68] := 0;
end;

procedure setTurbo_OFF;
var
  dummy:Byte;
begin
  dummy := port[68];
end;

function isCTS_ON: Boolean;
begin
  isCTS_ON := (port[MODEM_STATUS + comOffset] and 16) <> 0;
end;

procedure setDTR_ON;
begin
  port[MODEM_CONTROL + comOffset] := port[MODEM_CONTROL + comOffset] or $01;
end;

procedure setDTR_OFF;
begin
  port[MODEM_CONTROL + comOffset] := port[MODEM_CONTROL + comOffset] and $FE;
end;

procedure setRTS_ON;
begin
  port[MODEM_CONTROL + comOffset] := port[MODEM_CONTROL + comOffset] or $02;
end;

procedure setRTS_OFF;
begin
  port[MODEM_CONTROL + comOffset] := port[MODEM_CONTROL + comOffset] and $FD;
end;

procedure waitSend;
var status:Integer;
begin
  repeat
    status := port[LSR + comOffset] and $40;
  until (status = $40);
end;


procedure sendString(s:String);
var c:Integer;
begin
  for c:= 1 to length(s) do begin
    waitSend;
    port[TXDATA + comOffset] := ord(s[c]);
  end;
  port[TXDATA + comOffset] := 10;
end;

procedure sendByte(b:Byte);
begin
  waitSend;
  port[TXDATA + comOffset] := b;
end;


function getByte: Byte;
var status: Byte;
begin
  repeat
    status := port[LSR + comOffset] and $01;
  until (status = $01);
  getByte := port[TXDATA + comOffset];
end;

procedure chdir(hostDir:String);
var c,status:Integer;
begin
  writeln('Changing host dir to ' + hostDir);
  setDTR_ON;
  sendString('chdir');
  status := getByte;
  if status <> STATUS_OK then begin
    writeln('Unrecognized command chdir');
    write('Status code: ');writeln(status);
    exit;
  end;
  sendString(hostDir);
  waitSend;
  status := getByte;
  if status <> STATUS_OK then begin
    writeln('Error changing host directory, directory not found');
    exit;
  end
  else
    writeln('Host local directory set to ' + hostDir);
  setDTR_OFF;
end;

procedure put(localFileName:String;hostFileName:String);
var
    f:File;
    status:Integer;
    b:Byte;
    buffer: array[1..1024] of Byte;
    read,i:Integer;
begin

    Assign(f,localFileName);
    {$i-} Reset(f,1); {$i+}
    if ioresult <> 0 then begin
      writeln('Local file ' + localFileName + ' not found');
      exit;
    end;
    writeln('Sending file ' + localFileName + ' ...');
    setDTR_ON;
    sendString('put');
    status := getByte;
    if status <> STATUS_OK then begin
      writeln('Unrecognized command put');
      write('Status code: ');writeln(status);
      exit;
    end;
    sendString(hostFileName);
    status := getByte;
    if status <> STATUS_OK then begin
      writeln('Error while writing on host file');
      write('Status code: ');writeln(status);
      exit;
    end;
    setTurbo_ON;
    repeat
      BlockRead(f,buffer,SizeOf(buffer),read);
      write('.');
      for i := 1 to read do sendByte(buffer[i]);
    until (read = 0);
    waitSend;
    writeln('');
    setDTR_OFF;
    setTurbo_OFF;
    close(f);
    status := getByte;
    if status <> STATUS_OK then
      writeln('Error while writing file to host')
    else
      writeln('File ' + localFileName + ' transferred successfully.');
end;

procedure get(hostFileName:String;localFileName:String);
var
   f:File;
   status:Integer;
   buffer:array[1..1024] of byte;
   bufferCount,written:Integer;
   b:Byte;
begin
     Assign(f,localFileName);
     {$i-}Rewrite(f,1);{$i+}
     if ioresult <> 0 then begin
        writeln('Cannot write local file ' + localFileName);
        exit;
     end;
     writeln('Getting remote file ' + hostFileName + ' ...');
     sendString('get');
     status := getByte;
     if status <> STATUS_OK then begin
       writeln('Command get not recognized');
       write('Status code: ');writeln(status);
       exit;
     end;
     sendString(hostFileName);
     status := getByte;
     if status <> STATUS_OK then begin
       writeln('Error while reading host file');
       write('Status code: ');writeln(status);
       exit;
     end;
     bufferCount := 0;
     setDTR_ON;
     setRTS_ON;
     setTurbo_ON;
     while (not isCTS_ON) do;
     while (isCTS_ON) do begin
       b := getByte;

       inc(bufferCount);
       buffer[bufferCount] := b;
       if bufferCount = SizeOf(buffer) then begin
         setRTS_OFF;
         BlockWrite(f,buffer,bufferCount,written);
         bufferCount := 0;
         write('.');
         setRTS_ON;
       end;
     end;
     BlockWrite(f,buffer,bufferCount,written);
     writeln('.');
     close(f);
     setDTR_OFF;
     setTurbo_OFF;
     status := getByte;
     writeln('Host file ' + hostFileName + ' transferred locally successfully');
end;

begin
  if ParamCount < 2 then begin
    writeln('Usage com1|com2 chdir|get|put <parameters>');
    exit;
  end;

  if ParamStr(1) = 'com1' then comOffset := COM1_OFFSET
  else if ParamStr(1) = 'com2' then comOffset := 0
  else begin
    writeln('Bad serial port: expected com1 or com2');
    exit;
  end;


  setupSerial;

  if ParamStr(2) = 'chdir' then begin
    if ParamCount <> 3 then begin
      writeln('chdir command requires 1 parameter: host target directory');
      exit;
    end;
    chdir(ParamStr(3));
  end
  else
  if ParamStr(2) = 'put' then begin
    if ParamCount <> 4 and ParamCount <> 3 then begin
      writeln('put command requires 2 parameters: localfilepath [hostfilename]');
      exit;
    end;
    if ParamCount == 4 then
        put(ParamStr(3),ParamStr(4));
    else
        put(ParamStr(3),ParamStr(3));
  end
  else
  if ParamStr(2) = 'get' then begin
    if ParamCount <> 4 and ParamCount <> 3 then begin
      writeln('get command requires 2 parameters: hostfilename [localfilepath]');
      exit;
    end;
    if ParamCount == 4 then
        get(ParamStr(3),ParamStr(4));
    else
        get(ParamStr(3),ParamStr(3));
  end
  else
    writeln('Unrecognized command');
end.