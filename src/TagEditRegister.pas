//-----------------------------------------------------------------------------------
//  TTagEdit © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/TagEdit
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

uses TagEdit, TagCheckPopup;

procedure Register;
begin
  RegisterComponents('Common Controls', [TTagEdit]);
  RegisterComponents('Common Controls', [TCheckListButton]);
end;

initialization
  {$I TagEditRegister.lrs}

end.
