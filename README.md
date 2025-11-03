# TTagEdit Component

A versatile tag management control for Lazarus/FPC with advanced features.

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Build with: Lazarus](https://img.shields.io/badge/Build_with-Lazarus-blueviolet)](https://www.lazarus-ide.org/)
[![Platform: Windows Linux](https://img.shields.io/badge/Platform-Windows_Linux-yellow)](#)

![sample](TagEditSample.png)

## Features

- **🏷️ Tag Management**
  - Add tags by typing and pressing Enter
  - Tags with suffix
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
  - Random color for tag name if not defined
  - Tag color collection, assigns colors based on tag name match
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
| `Align` | TAlign | Control alignment within parent |
| `Anchors` | TAnchors | Anchor points for responsive layout |
| `AutoColorBrightness` | Integer | Brightness percentage for auto-generated tag colors (0-100) |
| `AutoSizeHeight` | Boolean | Auto-adjust height based on content |
| `BorderColor` | TColor | Component border color |
| `BorderWidth` | Integer | Component border width |
| `CloseButtons` | Boolean | Show close buttons on tags |
| `CloseButtonOnHover` | Boolean | Show close buttons only on hover |
| `Color` | TColor | Background color of the component |
| `EditMinWidth` | Integer | Minimum width of edit box |
| `Enabled` | Boolean | Enable/disable the component |
| `Font` | TFont | Font used for tags and edit box |
| `Height` | Integer | Component height |
| `Items` | TStringList | Collection of tag strings |
| `ParentFont` | Boolean | Use parent component's font |
| `PopupMenu` | TPopupMenu | Popup menu for the component |
| `ReadOnly` | Boolean | Read-only mode (no tag editing) |
| `RemoveConfirm` | Boolean | Show confirmation when removing tags |
| `RemoveConfirmMessage` | String | Message for remove confirmation dialog |
| `RemoveConfirmTitle` | String | Title for remove confirmation dialog |
| `RoundCorners` | Integer | Corner radius for tags |
| `ShowHint` | Boolean | Show hint tooltips |
| `Tag` | Integer | User-defined tag value |
| `TagBorderColor` | TColor | Border color of tags |
| `TagBorderWidth` | Integer | Border width of tags |
| `TagColor` | TColor | Background color of tags |
| `TagColors` | TTagColorItems | Collection of custom tag colors |
| `TagHoverColor` | TColor | Color on mouse hover |
| `TagHoverUnderline` | Boolean | Underline tag text on hover |
| `TagSuffixColor` | TColor | Background color for tag suffix (after colon) |
| `Visible` | Boolean | Control visibility |
| `Width` | Integer | Component width |

## Events

| Event | Type | Description |
|-------|------|-------------|
| `OnClick` | TNotifyEvent | Occurs when component is clicked |
| `OnDblClick` | TNotifyEvent | Occurs when component is double-clicked |
| `OnKeyDown` | TKeyEvent | Occurs when key is pressed |
| `OnKeyPress` | TKeyPressEvent | Occurs when key is pressed (character) |
| `OnKeyUp` | TKeyEvent | Occurs when key is released |
| `OnMouseDown` | TMouseEvent | Occurs when mouse button is pressed |
| `OnMouseEnter` | TNotifyEvent | Occurs when mouse enters component |
| `OnMouseLeave` | TNotifyEvent | Occurs when mouse leaves component |
| `OnMouseMove` | TMouseMoveEvent | Occurs when mouse moves over component |
| `OnMouseUp` | TMouseEvent | Occurs when mouse button is released |
| `OnTagAdd` | TTagEvent | Occurs when a new tag is added |
| `OnTagClick` | TTagEvent | Occurs when a tag is clicked |
| `OnTagPopup` | TTagPopupEvent | Occurs when context menu is requested for a tag |
| `OnTagRemove` | TTagEvent | Occurs when a tag is removed |

# Installation

## Method 1: Through IDE
1. Open Lazarus IDE
2. Go to **Package** → **Open Package File (.lpk)**
3. Navigate to `TagEdit\packages\TagEditPackage.lpk`
4. Click **Open**
5. In the package window, click **Compile**
6. After successful compilation, click **Use** → **Install**
7. Restart Lazarus IDE

## After Installation
- The component will appear in the palette under "Common Controls" tab
- You can now drag TTagEdit component to your forms
- Use Object Inspector to customize properties

## Sample usage

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
TagEdit1.EditMinWidth := 10;
```