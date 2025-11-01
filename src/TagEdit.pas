unit TagEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, Types, Forms, Dialogs, LCLType, LCLIntf;

type
  TTagEvent = procedure(Sender: TObject; const TagText: string) of object;

  TTagEdit = class(TCustomControl)
  private
    FTags: TStringList;
    FTagRects: array of TRect;

    FEdit: TEdit;
    FTagColor: TColor;
    FTagBorderColor: TColor;
    FBorderColor: TColor;
    FTagHoverColor: TColor;

    FTagBorderWidth: integer;
    FBorderWidth: integer;
    FRoundCorners: integer;
    FDragIndex: integer;
    FDropIndex: integer;
    FDragging: boolean;
    FUpdatingEdit: boolean;
    FEditMinWidth: integer;
    FHoverIndex: integer;
    FMouseDownPos: TPoint;
    FMouseDownIndex: integer;
    FRemoveConfirm: boolean;
    FRemoveConfirmMessage: string;
    FRemoveConfirmTitle: string;
    FFont: TFont;
    FParentFont: boolean;
    FReadOnly: boolean;

    FOnTagAdd: TTagEvent;
    FOnTagRemove: TTagEvent;
    FOnTagClick: TTagEvent;

    procedure EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    function GetTags: TStringList;
    procedure SetTags(Value: TStringList);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: boolean);
    procedure SetReadOnly(Value: boolean);
    procedure TagsChanged(Sender: TObject);
    procedure DrawTags;
    function GetTagHeight: integer;
    function GetTagRect(Index: integer): TRect;
    function TagAtPos(const P: TPoint): integer;
    procedure UpdateEditPosition;
    procedure UpdateHoverState(X, Y: integer);
    function CoalesceInt(const A, B: integer): integer;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    property TagHeight: integer read GetTagHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTag(const ATag: string);
    procedure RemoveTag(const ATag: string);
    property EditBox: TEdit read FEdit;
    procedure ParentFontChange(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure SetParent(AParent: TWinControl); override;
  published
    property Align;
    property Anchors;
    property Color default clWindow;
    property Enabled;
    property Visible;
    property ShowHint;
    property PopupMenu;
    property Height default 30;
    property Width default 300;
    property Tag default 0;
    property Font: TFont read FFont write SetFont;
    property TagColor: TColor read FTagColor write FTagColor default clBtnFace;
    property TagHoverColor: TColor read FTagHoverColor write FTagHoverColor default clSkyBlue;
    property TagBorderColor: TColor read FTagBorderColor write FTagBorderColor default clWindowFrame;
    property TagBorderWidth: integer read FTagBorderWidth write FTagBorderWidth default 0;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWindowFrame;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 0;
    property RoundCorners: integer read FRoundCorners write FRoundCorners default 5;
    property EditMinWidth: integer read FEditMinWidth write FEditMinWidth default 50;
    property RemoveConfirm: boolean read FRemoveConfirm write FRemoveConfirm default True;
    property RemoveConfirmTitle: string read FRemoveConfirmTitle write FRemoveConfirmTitle;
    property RemoveConfirmMessage: string read FRemoveConfirmMessage write FRemoveConfirmMessage;
    property ParentFont: boolean read FParentFont write SetParentFont default True;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;

    property Tags: TStringList read GetTags write SetTags;

    property OnTagAdd: TTagEvent read FOnTagAdd write FOnTagAdd;
    property OnTagRemove: TTagEvent read FOnTagRemove write FOnTagRemove;
    property OnTagClick: TTagEvent read FOnTagClick write FOnTagClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

{ TTagEdit }

constructor TTagEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 30;
  Width := 300;
  Tag := 0;
  Color := clWindow;
  ParentColor := False;

  FReadOnly := False;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FParentFont := True;

  FTags := TStringList.Create;
  FTags.Add('Lazarus');
  FTags.Add('Free Pascal');

  FTags.OnChange := @TagsChanged;

  FTagColor := clBtnFace;
  FTagHoverColor := clSkyBlue;
  FTagBorderColor := clWindowFrame;
  FTagBorderWidth := 0;
  FBorderColor := clWindowFrame;
  FEditMinWidth := 50;
  FRoundCorners := 5;

  // Create inner edit control
  FEdit := TEdit.Create(Self);
  if not (csDesigning in ComponentState) then
  begin
    FEdit.Parent := Self;
    FEdit.DoubleBuffered := True;
    FEdit.ParentFont := True;
    FEdit.BorderStyle := bsNone;
    FEdit.OnKeyDown := @EditKeyDown;
    FEdit.OnExit := @EditExit;
    FEdit.Color := Color;
    FEdit.Left := 4;
    FEdit.Top := 4;
  end;

  FRemoveConfirm := True;
  FRemoveConfirmMessage := 'Are you sure you want to remove tag';
  FRemoveConfirmTitle := 'Remove tag';

  FDragging := False;
  FDragIndex := -1;
  FDropIndex := -1;
  FUpdatingEdit := False;
  FHoverIndex := -1;
  FMouseDownIndex := -1;
end;

destructor TTagEdit.Destroy;
begin
  FTags.Free;
  FFont.Free;
  FEdit.Free;
  inherited Destroy;
end;

function TTagEdit.GetTags: TStringList;
begin
  Result := FTags;
end;

procedure TTagEdit.SetTags(Value: TStringList);
begin
  FTags.Assign(Value);
  Invalidate;
end;

procedure TTagEdit.SetFont(Value: TFont);
begin
  if Value <> nil then
  begin
    FFont.Assign(Value);
    FEdit.Font.Assign(Value);
    FParentFont := False;
    Invalidate;
  end;
end;

procedure TTagEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FParentFont and (AParent <> nil) then
  begin
    FFont.Assign(AParent.Font);
    FEdit.Font.Assign(AParent.Font);
    SetParentFont(True);
  end;
end;

procedure TTagEdit.SetParentFont(Value: boolean);
begin
  FParentFont := Value;

  if FParentFont and (Parent <> nil) then
  begin
    // Detach previous event
    if Assigned(FFont.OnChange) then
      FFont.OnChange := nil;

    // Copy parent's font
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);

    // Subscribe to parent's font change manually
    Parent.Font.OnChange := @ParentFontChange;

    // Subscribe to font change
    FFont.OnChange := @FontChanged;
  end
  else
  begin
    // Remove parent font change hook
    if Assigned(Parent) then
      Parent.Font.OnChange := nil;

    // Copy font
    FEdit.Font.Assign(FFont);

    // Subscribe to font change
    FFont.OnChange := @FontChanged;
  end;
end;

procedure TTagEdit.ParentFontChange(Sender: TObject);
begin
  if FParentFont and (Parent <> nil) then
  begin
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);
    Invalidate;
  end;
end;

procedure TTagEdit.FontChanged(Sender: TObject);
begin
  inherited;
  FEdit.Font.Assign(Font);
  if (Assigned(Parent)) and (not FFont.IsEqual(Parent.Font)) then
    FParentFont := False;
  Invalidate;
end;

procedure TTagEdit.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  FEdit.ReadOnly := Value;
  Invalidate;
end;

procedure TTagEdit.TagsChanged(Sender: TObject);
begin
  FHoverIndex := -1; // Reset hover state when tags change
  Invalidate;
end;

procedure TTagEdit.AddTag(const ATag: string);
begin
  if (ATag <> string.Empty) and (FTags.IndexOf(ATag) = -1) then
  begin
    FTags.Add(ATag);
    FEdit.Text := string.Empty;
    if Assigned(FOnTagAdd) then
      FOnTagAdd(Self, ATag);
    Invalidate;
  end;
end;

procedure TTagEdit.RemoveTag(const ATag: string);
var
  i: integer;
begin
  i := FTags.IndexOf(ATag);
  if i >= 0 then
  begin
    FTags.Delete(i);
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, ATag);
    Invalidate;
  end;
end;

function TTagEdit.GetTagHeight: integer;
begin
  Result := CoalesceInt(Font.Size, Screen.SystemFont.Size) * 2 + 5;
end;

function TTagEdit.GetTagRect(Index: integer): TRect;
begin
  if (Index >= 0) and (Index <= High(FTagRects)) then
    Result := FTagRects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TTagEdit.UpdateEditPosition;
var
  LastRect: TRect;
  NewLeft, NewTop: integer;
  AvailWidth: integer;
begin
  if FUpdatingEdit then Exit;

  FUpdatingEdit := True;
  try
    // Calculate available width considering borders
    AvailWidth := ClientWidth - 8;
    if FBorderWidth > 0 then
      Dec(AvailWidth, 2 * FBorderWidth);

    if FTags.Count > 0 then
    begin
      LastRect := GetTagRect(FTags.Count - 1);

      // Check if sufficient space exists for Edit after last tag
      if (LastRect.Right + 4 + FEditMinWidth) <= AvailWidth then
      begin
        // If space permits, place on the same line
        NewLeft := LastRect.Right + 4;
        NewTop := LastRect.Top;
      end
      else
      begin
        // If insufficient space - move to new line
        NewLeft := 4;
        NewTop := LastRect.Bottom + 4;
      end;
    end
    else
    begin
      NewLeft := 4;
      NewTop := 4;
    end;

    // Set position only when changed
    if (FEdit.Left <> NewLeft) or (FEdit.Top <> NewTop) then
    begin
      FEdit.Left := NewLeft;
      FEdit.Top := NewTop;
    end;

    // Set Edit width
    FEdit.Width := ClientWidth - FEdit.Left - 4;
    if FEdit.Width < FEditMinWidth then
      FEdit.Width := FEditMinWidth;
    FEdit.Height := TagHeight;
  finally
    FUpdatingEdit := False;
  end;
end;

function TTagEdit.CoalesceInt(const A, B: integer): integer;
begin
  if A <> 0 then
    Result := A
  else
    Result := B;
end;

procedure TTagEdit.DrawTags;
var
  i: integer;
  R: TRect;
  s: string;
  X, Y, W, H, M: integer;
  AvailWidth: integer;
  CurrentTagColor: TColor;
begin
  // Calculate available width considering border
  AvailWidth := ClientWidth - 8;
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  Canvas.Font.Assign(Font);
  SetLength(FTagRects, FTags.Count);

  X := 4;
  Y := 4;
  H := TagHeight;

  for i := 0 to FTags.Count - 1 do
  begin
    s := FTags[i];
    M := CoalesceInt(Font.Size, Screen.SystemFont.Size) * 2 + 6;
    W := Canvas.TextWidth(s) + M;

    // Move to next line if tag doesn't fit
    if (X + W) > AvailWidth then
    begin
      X := 4;
      Y := Y + H + 4;
    end;

    R := Rect(X, Y, X + W, Y + H);
    FTagRects[i] := R;

    // Determine tag color (hover or normal)
    if (i = FHoverIndex) and (FTagHoverColor <> clNone) then
      CurrentTagColor := FTagHoverColor
    else
      CurrentTagColor := FTagColor;

    // Draw background
    Canvas.Pen.Width := FTagBorderWidth;
    Canvas.Pen.Color := FTagBorderColor;
    Canvas.Brush.Color := CurrentTagColor;
    if FTagBorderWidth <= 0 then
      Canvas.Pen.Style := psClear
    else
      Canvas.Pen.Style := psSolid;

    Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);

    // Draw tag text
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(R.Left + 6, R.Top + 2, s);

    // Draw '×' button
    if (not FReadOnly) then
    begin
      Canvas.Font.Color := clGray;
      M := Round(CoalesceInt(Font.Size, Screen.SystemFont.Size) * 1.3) + 2;
      Canvas.TextOut(R.Right - M, R.Top + 2, '×');
    end;

    Inc(X, W + 4);
  end;

  if not (csDesigning in ComponentState) then
    UpdateEditPosition;
end;

procedure TTagEdit.Paint;
var
  BorderRect, R: TRect;
begin
  // Draw component background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Draw component border if needed
  if FBorderWidth > 0 then
  begin
    Canvas.Pen.Width := FBorderWidth;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Style := psSolid;
    BorderRect := ClientRect;
    Canvas.Rectangle(BorderRect);
  end;

  DrawTags;

  // Draw drag indicator
  if not (csDesigning in ComponentState) and FDragging and (FDropIndex >= 0) and (FDropIndex <= FTags.Count) then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clHighlight;
    Canvas.Pen.Style := psSolid;
    if FDropIndex < FTags.Count then
    begin
      R := GetTagRect(FDropIndex);
      Canvas.MoveTo(R.Left - 2, R.Top);
      Canvas.LineTo(R.Left - 2, R.Bottom);
    end
    else if FTags.Count > 0 then
    begin
      R := GetTagRect(FTags.Count - 1);
      Canvas.MoveTo(R.Right + 2, R.Top);
      Canvas.LineTo(R.Right + 2, R.Bottom);
    end
    else
    begin
      // No tags - show indicator at start
      Canvas.MoveTo(2, 4);
      Canvas.LineTo(2, 4 + TagHeight);
    end;
  end;
end;

function TTagEdit.TagAtPos(const P: TPoint): integer;
var
  i: integer;
  R: TRect;
begin
  for i := 0 to FTags.Count - 1 do
  begin
    R := GetTagRect(i);
    if PtInRect(R, P) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TTagEdit.UpdateHoverState(X, Y: integer);
var
  NewHoverIndex: integer;
begin
  NewHoverIndex := TagAtPos(Point(X, Y));

  if NewHoverIndex <> FHoverIndex then
  begin
    FHoverIndex := NewHoverIndex;
    Invalidate;
  end;
end;

procedure TTagEdit.EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ATag: string;
begin
  if csDesigning in ComponentState then exit;

  // Enter adds a new tag
  if Key = VK_RETURN then
  begin
    if FEdit.Text <> string.Empty then
      AddTag(FEdit.Text);
    Key := 0;
  end;

  // Backspace removes last tag if edit is empty
  if (Key = VK_BACK) and (FEdit.Text = string.Empty) and (FTags.Count > 0) then
  begin
    ATag := FTags[FTags.Count - 1];
    FTags.Delete(FTags.Count - 1);
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, ATag);
    Invalidate;
  end;
end;

procedure TTagEdit.EditExit(Sender: TObject);
begin
  // When leaving the edit, add tag if not empty
  if FEdit.Text <> string.Empty then
    AddTag(FEdit.Text);
end;

procedure TTagEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  idx: integer;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if Button = mbLeft then
  begin
    idx := TagAtPos(Point(X, Y));
    FMouseDownIndex := idx;
    FMouseDownPos := Point(X, Y);

    if (not FReadOnly) and (idx >= 0) then
    begin
      R := GetTagRect(idx);
      // Click near right edge removes the tag - do it immediately
      if (X > R.Right - 16) and ((not RemoveConfirm) or (MessageDlg(FRemoveConfirmTitle, FRemoveConfirmMessage +
        ' "' + FTags[idx] + '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)) then
      begin
        RemoveTag(FTags[idx]);
        FMouseDownIndex := -1; // Reset since we handled it
      end;
    end;
    FEdit.SetFocus;
  end;
end;

procedure TTagEdit.MouseMove(Shift: TShiftState; X, Y: integer);
var
  idx: integer;
  NewDropIndex: integer;
  DragThreshold: integer;
  i: integer;
  R: TRect;
  ClosestIndex: integer;
  MinDistance: integer;
  Distance: integer;
  FoundInTag: boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if csDesigning in ComponentState then exit;

  // Update hover state
  if not FDragging then
    UpdateHoverState(X, Y);

  // Start dragging only if mouse moved beyond threshold and we have a valid tag index
  if not FReadOnly and not FDragging and (ssLeft in Shift) and (FMouseDownIndex >= 0) then
  begin
    DragThreshold := 5; // pixels
    if (Abs(X - FMouseDownPos.X) > DragThreshold) or (Abs(Y - FMouseDownPos.Y) > DragThreshold) then
    begin
      FDragging := True;
      FDragIndex := FMouseDownIndex;
      FDropIndex := FMouseDownIndex;
      Invalidate;
    end;
  end;

  if FDragging then
  begin
    // First check if cursor is directly over a tag
    idx := TagAtPos(Point(X, Y));
    FoundInTag := (idx >= 0);

    if FoundInTag then
    begin
      // If cursor is over a tag, place before that tag
      NewDropIndex := idx;
    end
    else
    begin
      // Overfloaw last tag, place at the end
      R := GetTagRect(FTags.Count - 1);
      if (Y >= R.Bottom) or ((Y >= R.Top) and (X > R.Right)) then
        NewDropIndex := FTags.Count
      else
      begin
        // If cursor is between tags, find the closest tag
        ClosestIndex := -1;
        MinDistance := MaxInt;

        for i := 0 to FTags.Count - 1 do
        begin
          R := GetTagRect(i);

          // Calculate distance to tag center
          Distance := Abs(X - R.Left) + Abs(Y - (R.Top + R.Height div 2));

          if Distance < MinDistance then
          begin
            MinDistance := Distance;
            ClosestIndex := i;
          end;
        end;

        if ClosestIndex >= 0 then
        begin
          NewDropIndex := ClosestIndex;
        end
        else
        begin
          // No tags found, place at the end
          NewDropIndex := FTags.Count;
        end;
      end;
    end;

    // Only update and invalidate if position changed
    if NewDropIndex <> FDropIndex then
    begin
      FDropIndex := NewDropIndex;
      Invalidate;
    end;
  end;
end;

procedure TTagEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TempTag: string;
  idx: integer;
  R: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if Button = mbLeft then
  begin
    // Handle click event if we didn't drag and it's the same tag we pressed
    if not FDragging and (FMouseDownIndex >= 0) then
    begin
      idx := TagAtPos(Point(X, Y));
      if (idx = FMouseDownIndex) and (idx >= 0) then
      begin
        R := GetTagRect(idx);
        // Don't trigger click for remove button area (already handled in MouseDown)
        if not (X > R.Right - 16) then
        begin
          // Generate OnTagClick event
          if Assigned(FOnTagClick) then
            FOnTagClick(Self, FTags[idx]);
        end;
      end;
    end;

    // Handle drag completion
    if FDragging then
    begin
      FDragging := False;
      if (FDragIndex >= 0) and (FDropIndex >= 0) and (FDragIndex <> FDropIndex) then
      begin
        // Adjust drop index if dragging forward
        if FDropIndex > FDragIndex then
          Dec(FDropIndex);

        TempTag := FTags[FDragIndex];
        FTags.Delete(FDragIndex);
        if FDropIndex >= FTags.Count then
          FTags.Add(TempTag)
        else
          FTags.Insert(FDropIndex, TempTag);
      end;
      FDragIndex := -1;
      FDropIndex := -1;
      Invalidate;
    end;

    // Reset mouse down state
    FMouseDownIndex := -1;
  end;
end;

procedure TTagEdit.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then exit;

  if FHoverIndex <> -1 then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

end.
