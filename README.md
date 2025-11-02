# TTagEdit Component

A versatile tag management control for Lazarus/FPC with advanced features.

![sample](TagEditSample.png)

## Features

- **🏷️ Tag Management**
  - Add tags by typing and pressing Enter
  - Remove tags with click confirmation
  - Prevent duplicate tags

- **🔄 Drag & Drop**
  - Rearrange tags via intuitive drag-and-drop
  - Visual drop indicators
  - Smooth repositioning

- **🎨 Customization**
  - Custom tag colors and hover effects
  - Adjustable border styles and widths
  - Rounded corners support
  - Font and size customization

- **📐 Smart Layout**
  - Automatic line wrapping
  - Dynamic edit box positioning
  - Optional auto-height adjustment
  - Responsive tag placement

- **⚡ Interactive Features**
  - Hover effects with customizable colors
  - Click events for individual tags
  - Remove confirmation dialogs
  - Read-only mode support

- **⌨️ Keyboard Support**
  - Enter to add tags
  - Backspace to remove last tag
  - Focus management

## Properties

| Property | Type | Description |
|----------|------|-------------|
| `Items` | TStringList | Collection of tag strings |
| `AutoSizeHeight` | Boolean | Auto-adjust height based on content |
| `TagColor` | TColor | Background color of tags |
| `TagHoverColor` | TColor | Color on mouse hover |
| `TagBorderColor` | TColor | Border color of tags |
| `TagBorderWidth` | Integer | Border width of tags |
| `BorderColor` | TColor | Component border color |
| `BorderWidth` | Integer | Component border width |
| `RoundCorners` | Integer | Corner radius for tags |
| `EditMinWidth` | Integer | Minimum width of edit box |
| `RemoveConfirm` | Boolean | Show confirmation when removing tags |
| `RemoveConfirmTitle` | String | Title for remove confirmation dialog |
| `RemoveConfirmMessage` | String | Message for remove confirmation dialog |
| `ParentFont` | Boolean | Use parent component's font |
| `ReadOnly` | Boolean | Read-only mode (no tag editing) |
| `Font` | TFont | Font used for tags and edit box |

## Events

- `OnTagAdd` - Fired when a tag is added
- `OnTagRemove` - Fired when a tag is removed  
- `OnTagClick` - Fired when a tag is clicked

## Usage

```pascal
// Add tags programmatically
TagEdit1.Tags.Add('Lazarus');
TagEdit1.Tags.Add('FreePascal');

// Enable auto-height
TagEdit1.AutoSizeHeight := True;

// Customize appearance
TagEdit1.TagColor := clSkyBlue;
TagEdit1.TagHoverColor := clLightBlue;
TagEdit1.RoundCorners := 8;
TagEdit1.TagBorderColor := clNavy;
TagEdit1.TagBorderWidth := 1;

// Configure remove confirmation
TagEdit1.RemoveConfirm := True;
TagEdit1.RemoveConfirmTitle := 'Delete Tag';
TagEdit1.RemoveConfirmMessage := 'Are you sure you want to remove this tag?';

// Set minimum edit width
TagEdit1.EditMinWidth := 80;
```