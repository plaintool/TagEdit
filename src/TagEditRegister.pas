unit TagEditRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources;

procedure Register;

implementation

uses TagEdit;

procedure Register;
begin
  RegisterComponents('Common Controls', [TTagEdit]);
end;

initialization
  {$I TagEditRegister.lrs}

end.
