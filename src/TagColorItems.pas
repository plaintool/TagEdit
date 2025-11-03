unit TagColorItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TTagColorItem }
  // Represents one pair: Tag name -> Color
  TTagColorItem = class(TCollectionItem)
  private
    FTagName: string;
    FColor: TColor;
    procedure SetTag(const AValue: string);
    procedure SetColor(AValue: TColor);
  published
    // Tag name
    property TagName: string read FTagName write SetTag;
    // Color associated with the tag
    property Color: TColor read FColor write SetColor default clNone;
  end;

  { TTagColorItems }
  // Collection container for TTagColorItem. Owned by a component (owner = component/TPersistent)
  TTagColorItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TTagColorItem;
    procedure SetItem(Index: Integer; const Value: TTagColorItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add(const ATag: string; AColor: TColor): TTagColorItem;
    function IndexOf(const ATag: string): Integer;
    property Items[Index: Integer]: TTagColorItem read GetItem write SetItem; default;
  end;

implementation

{ TTagColorItem }

procedure TTagColorItem.SetTag(const AValue: string);
begin
  if FTagName = AValue then Exit;
  FTagName := AValue;
  // notify owner collection that item changed (will trigger streaming / Invalidate if needed)
  Changed(False);
end;

procedure TTagColorItem.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Changed(False);
end;

{ TTagColorItems }

constructor TTagColorItems.Create(AOwner: TPersistent);
begin
  // Create collection of TTagColorItem items
  inherited Create(AOwner, TTagColorItem);
end;

function TTagColorItems.GetItem(Index: Integer): TTagColorItem;
begin
  Result := TTagColorItem(inherited Items[Index]);
end;

procedure TTagColorItems.SetItem(Index: Integer; const Value: TTagColorItem);
begin
  Items[Index].Assign(Value);
end;

function TTagColorItems.Add(const ATag: string; AColor: TColor): TTagColorItem;
begin
  Result := TTagColorItem(inherited Add);
  Result.FTagName := ATag;
  Result.FColor := AColor;
  Result.Changed(False);
end;

function TTagColorItems.IndexOf(const ATag: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].TagName = ATag then
      Exit(i);
  Result := -1;
end;

end.

