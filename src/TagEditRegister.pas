//-----------------------------------------------------------------------------------
//  TTagEdit © 2025 by Alexander Tverskoy
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

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
