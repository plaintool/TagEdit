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
    FColor: TColor;
    FTagColor: TColor;
    FTagBorderColor: TColor;
    FBorderColor: TColor;
    FTagHoverColor: TColor;

    FAutoSizeHeight: boolean;
    FAllowReorder: boolean;
    FReadOnly: boolean;
    FEnabled: boolean;

    FBorderWidth: integer;
    FRoundCorners: integer;
    FTagBorderWidth: integer;
    FEditMinWidth: integer;

    FDragging: boolean;
    FDragIndex: integer;
    FDropIndex: integer;
    FUpdatingEdit: boolean;
    FHoverIndex: integer;
    FMouseDownPos: TPoint;
    FMouseDownIndex: integer;

    FRemoveConfirm: boolean;
    FRemoveConfirmMessage: string;
    FRemoveConfirmTitle: string;

    FFont: TFont;
    FParentFont: boolean;

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
    function RemovalConfirmed(idx: integer): boolean;
    procedure DrawTags;
    function GetTagHeight: integer;
    function GetTagRect(Index: integer): TRect;
    function TagAtPos(const P: TPoint): integer;
    procedure UpdateEditPosition;
    procedure UpdateHoverState(X, Y: integer);
    function CoalesceInt(const A, B: integer; const C: integer = 0): integer;
    procedure SetAutoSizeHeight(Value: boolean);
    procedure UpdateAutoHeight;
    function Scale(const AValue: integer): integer;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure SetEnabled(Value: boolean); override;
    procedure Resize; override;
    procedure SetColor(Value: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTag(const ATag: string);
    procedure RemoveTag(const ATag: string);
    property EditBox: TEdit read FEdit;
    procedure ParentFontChange(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure SetParent(AParent: TWinControl); override;
    function CalculateAutoHeight: integer;
    procedure ScaleFontsPPI(const AToPPI: integer; const AProportion: double); override;
    procedure FixDesignFontsPPI(const ADesignTimePPI: integer); override;
  published
    property Align;
    property Anchors;
    property Visible;
    property ShowHint;
    property PopupMenu;
    property Height default 32;
    property Width default 300;
    property Tag default 0;
    property Color read FColor write SetColor default clWindow;
    property Font: TFont read FFont write SetFont;
    property AutoSizeHeight: boolean read FAutoSizeHeight write SetAutoSizeHeight default False;
    property AllowReorder: boolean read FAllowReorder write FAllowReorder default True;
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
    property Enabled: boolean read FEnabled write SetEnabled;

    property Items: TStringList read GetTags write SetTags;

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
  Height := Scale(32);
  Width := Scale(300);
  FColor := clWindow;
  ParentColor := False;

  FReadOnly := False;
  FEnabled := True;
  FAutoSizeHeight := False;
  FAllowReorder := True;
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
  FEditMinWidth := Scale(50);
  FRoundCorners := Scale(5);

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
  UpdateAutoHeight;
end;

procedure TTagEdit.SetColor(Value: TColor);
begin
  inherited;
  FColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := Value;
end;

procedure TTagEdit.SetFont(Value: TFont);
begin
  if Value <> nil then
  begin
    FFont.Assign(Value);
    FEdit.Font.Assign(Value);
    FParentFont := False;
    Invalidate;
    UpdateAutoHeight;
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
  Invalidate;
  UpdateAutoHeight;
end;

procedure TTagEdit.ParentFontChange(Sender: TObject);
begin
  if FParentFont and (Parent <> nil) then
  begin
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TTagEdit.FontChanged(Sender: TObject);
begin
  inherited;
  FEdit.Font.Assign(Font);
  if (Assigned(Parent)) and (not FFont.IsEqual(Parent.Font)) and (FFont.Size > 0) then
    FParentFont := False;
  Invalidate;
  UpdateAutoHeight;
end;

function TTagEdit.Scale(const AValue: integer): integer;
begin
  Result := Scale96ToScreen(AValue);
end;

procedure TTagEdit.ScaleFontsPPI(const AToPPI: integer; const AProportion: double);
begin
  inherited ScaleFontsPPI(AToPPI, AProportion);
  if FFont.Size = 0 then FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoScaleFontPPI(FFont, AToPPI, AProportion);
  if (Assigned(Parent)) and (FParentFont) then
    DoScaleFontPPI(Parent.Font, AToPPI, AProportion);
end;

procedure TTagEdit.FixDesignFontsPPI(const ADesignTimePPI: integer);
begin
  inherited FixDesignFontsPPI(ADesignTimePPI);
  if FFont.Size = 0 then FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoFixDesignFontPPI(FFont, ADesignTimePPI);
  if (Assigned(Parent)) and (FParentFont) then
    DoFixDesignFontPPI(Parent.Font, ADesignTimePPI);
end;

procedure TTagEdit.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  FEdit.Visible := not Value and FEnabled;
  Invalidate;
end;

procedure TTagEdit.SetEnabled(Value: boolean);
begin
  inherited;
  FEnabled := Value;
  FEdit.Visible := Value and not FReadOnly;
end;

procedure TTagEdit.SetAutoSizeHeight(Value: boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    if FAutoSizeHeight then
      UpdateAutoHeight;
  end;
end;

procedure TTagEdit.TagsChanged(Sender: TObject);
begin
  FHoverIndex := -1; // Reset hover state when Items change
  Invalidate;
  UpdateAutoHeight;
end;

function TTagEdit.RemovalConfirmed(idx: integer): boolean;
begin
  if not RemoveConfirm then
    exit(True);
  Result := MessageDlg(FRemoveConfirmTitle, FRemoveConfirmMessage + ' "' + FTags[idx] + '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
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
    UpdateAutoHeight;
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
    UpdateAutoHeight;
  end;
end;

function TTagEdit.GetTagHeight: integer;
begin
  Result := Scale(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 2 + 6);
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
    AvailWidth := ClientWidth - Scale(8);
    if FBorderWidth > 0 then
      Dec(AvailWidth, 2 * FBorderWidth);

    if FTags.Count > 0 then
    begin
      LastRect := GetTagRect(FTags.Count - 1);

      // Check if sufficient space exists for Edit after last tag
      if (LastRect.Right + Scale(4) + FEditMinWidth) <= AvailWidth then
      begin
        // If space permits, place on the same line
        NewLeft := LastRect.Right + Scale(4);
        NewTop := LastRect.Top;
      end
      else
      begin
        // If insufficient space - move to new line
        NewLeft := Scale(4);
        NewTop := LastRect.Bottom + Scale(4);
      end;
    end
    else
    begin
      NewLeft := Scale(4);
      NewTop := Scale(4);
    end;

    // Set position only when changed
    if (FEdit.Left <> NewLeft) or (FEdit.Top <> NewTop) then
    begin
      FEdit.Left := NewLeft;
      FEdit.Top := NewTop;
    end;

    // Set Edit width
    FEdit.Width := ClientWidth - FEdit.Left - Scale(4);
    if FEdit.Width < FEditMinWidth then
      FEdit.Width := FEditMinWidth;
    FEdit.Height := GetTagHeight;
  finally
    FUpdatingEdit := False;
  end;
end;

function TTagEdit.CoalesceInt(const A, B: integer; const C: integer = 0): integer;
begin
  if A > 0 then
    Result := A
  else
  if B > 0 then
    Result := B
  else
    Result := C;
end;

function TTagEdit.CalculateAutoHeight: integer;
var
  I: integer;
  X, Y, W, H: integer;
  AvailWidth: integer;
  LastRect: TRect;
  EditBottom: integer;
begin
  if FTags.Count = 0 then
  begin
    // Minimum height for empty control (edit box + padding)
    Result := Scale(4) + GetTagHeight + Scale(4);
    if FBorderWidth > 0 then
      Inc(Result, 2 * FBorderWidth);
    Exit;
  end;

  // Calculate available width
  AvailWidth := ClientWidth - Scale(8);
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  Canvas.Font.Assign(Font);
  H := GetTagHeight;

  X := Scale(4);
  Y := Scale(4);

  // Simulate tag layout to find bottom position
  for I := 0 to FTags.Count - 1 do
  begin
    W := Canvas.TextWidth(FTags[I]) + GetTagHeight;

    // Wrap to next line if tag doesn't fit
    if (I > 0) and ((X + W) > AvailWidth) then
    begin
      X := Scale(4);
      Y := Y + H + Scale(4);
    end;

    LastRect := Rect(X, Y, X + W, Y + H);
    Inc(X, W + Scale(4));
  end;

  // Calculate edit box position
  if (LastRect.Right + 4 + FEditMinWidth) <= AvailWidth then
    EditBottom := LastRect.Bottom + Scale(4) // Edit on same line
  else
    EditBottom := LastRect.Bottom + Scale(4) + H + Scale(4); // Edit on new line

  // Return total height including borders
  Result := EditBottom;
  if FBorderWidth > 0 then
    Inc(Result, 2 * FBorderWidth);
end;

procedure TTagEdit.UpdateAutoHeight;
begin
  if FAutoSizeHeight and (not (Align in [alClient, alRight, alLeft])) then
  begin
    Height := CalculateAutoHeight;
  end;
end;

procedure TTagEdit.Resize;
begin
  inherited Resize;
  UpdateAutoHeight;
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
  AvailWidth := ClientWidth - Scale(8);
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  Canvas.Font.Assign(Font);
  SetLength(FTagRects, FTags.Count);

  X := Scale(4);
  Y := Scale(4);
  H := GetTagHeight;

  for i := 0 to FTags.Count - 1 do
  begin
    s := FTags[i];
    M := GetTagHeight;
    W := Canvas.TextWidth(s) + M;

    // Move to next line if tag doesn't fit
    if (i > 0) and ((X + W) > AvailWidth) then
    begin
      X := Scale(4);
      Y := Y + H + Scale(4);
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
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := Font.Color;

    if (FReadOnly) then
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2)
    else
      M := 0;

    Canvas.TextOut(R.Left + Scale(6) + M div 2, R.Top + Scale(2), s);

    // Draw '×' button
    if (not FReadOnly) then
    begin
      Canvas.Font.Color := clGray;
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
      Canvas.TextOut(R.Right - M, R.Top + Scale(2), '×');
    end;

    Inc(X, W + Scale(4));
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
      Canvas.MoveTo(R.Left - Scale(2), R.Top);
      Canvas.LineTo(R.Left - Scale(2), R.Bottom);
    end
    else if FTags.Count > 0 then
    begin
      R := GetTagRect(FTags.Count - 1);
      Canvas.MoveTo(R.Right + Scale(2), R.Top);
      Canvas.LineTo(R.Right + Scale(2), R.Bottom);
    end
    else
    begin
      // No Items - show indicator at start
      Canvas.MoveTo(Scale(2), Scale(4));
      Canvas.LineTo(Scale(2), Scale(4) + GetTagHeight);
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
      AddTag(Trim(FEdit.Text));
    Key := 0;
  end;

  // Backspace removes last tag if edit is empty
  if (Key = VK_BACK) and (FEdit.Text = string.Empty) and (FTags.Count > 0) and RemovalConfirmed(FTags.Count - 1) then
  begin
    ATag := FTags[FTags.Count - 1];
    FTags.Delete(FTags.Count - 1);
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, ATag);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TTagEdit.EditExit(Sender: TObject);
begin
  // When leaving the edit, add tag if not empty
  if FEdit.Text <> string.Empty then
    AddTag(Trim(FEdit.Text));
end;

procedure TTagEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  idx, M: integer;
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
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
      // Click near right edge removes the tag - do it immediately
      if (X > R.Right - M) and RemovalConfirmed(idx) then
      begin
        RemoveTag(FTags[idx]);
        FMouseDownIndex := -1; // Reset since we handled it
      end;
    end;
    if FEdit.Visible and FEdit.CanFocus then
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
  if FAllowReorder and not FReadOnly and not FDragging and (ssLeft in Shift) and (FMouseDownIndex >= 0) then
  begin
    DragThreshold := Scale(5); // pixels
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
        // If cursor is between Items, find the closest tag
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
          // No Items found, place at the end
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
  idx, M: integer;
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
        if ReadOnly then
          M := 0
        else
          M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
        // Don't trigger click for remove button area (already handled in MouseDown)
        if not (X > R.Right - M) then
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
      UpdateAutoHeight;
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
