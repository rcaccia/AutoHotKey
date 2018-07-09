; IMPORTANT INFO ABOUT GETTING STARTED: Lines that start with a
; semicolon, such as this one, are comments.  They are not executed.

; This script has a special filename and path because it is automatically
; launched when you run the program directly.  Also, any text file whose
; name ends in .ahk is associated with the program, which means that it
; can be launched simply by double-clicking it.  You can have as many .ahk
; files as you want, located in any folder.  You can also run more than
; one ahk file simultaneously and each will get its own tray icon.

; SAMPLE HOTKEYS: Below are two sample hotkeys.  The first is Win+Z and it
; launches a web site in the default browser.  The second is Control+Alt+N
; and it launches a new Notepad window (or activates an existing one).  To
; try out these hotkeys, run AutoHotkey again, which will load this file.
;-------------------------------------------------------------------------

;--------------------------------------------------------------------
; USEFUL KEYS TO REMEMBER
;
; ^ = [Ctrl]
; ! = [Alt]
; + = [Shift]
; # = [Windows button]
;--------------------------------------------------------------------


^!+h::Reload

; #z::Run www.autohotkey.com/docs/AutoHotkey.htm
; #z::Run https://qalogin.gs.com

#g::Run https://www.google.com


;--------------------------------------------------------------------
^

^/::Send {AppsKey}
^\::Send {AppsKey}


; Open a command prompt
#c::   Run %ComSpec%

;--------------------------------------------------------------------
; Note: From now on whenever you run AutoHotkey directly, this script
; will be loaded.  So feel free to customize it to suit your needs.

; Please read the QUICK-START TUTORIAL near the top of the help file.
; It explains how to perform common automation tasks such as sending
; keystrokes and mouse clicks.  It also explains more about hotkeys.

;------------------------------------
;
; WINDOW CONTROLS
;
;------------------------------------

; Gets the edge that the taskbar is docked to.  Returns:
;   "top"
;   "right"
;   "bottom"
;   "left"
GetTaskbarEdge() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,

;  MsgBox, Taskbar : TX: %TX% -- TY: %TY% -- TW: %TW% -- TH: %TH%

  if (TW = A_ScreenWidth) { ; Vertical Taskbar
    if (TY = 0) {
      return "top"
    } else {
      return "bottom"
    }
  } else { ; Horizontal Taskbar
    if (TX = 0) {
      return "left"
    } else {
      return "right"
    }
  }
}

GetScreenTop() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

  if (TaskbarEdge = "top") {
    return TH
  } else {
    return 0
  }
}

GetScreenLeft() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

  if (TaskbarEdge = "left") {
    return TW
  } else {
    return 0
  }
}

GetScreenWidth() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

;MsgBox, Monitor info: ( %TX% , %TY% , %TW% , %TH%, %TaskbarEdge%, %A_ScreenWidth% )


;  if (TaskbarEdge = "top" or TaskbarEdge = "bottom")
    return A_ScreenWidth

    return A_ScreenWidth - TW
}

GetScreenHeight() {

SysGet, MonitorWorkArea, MonitorWorkArea, 1
;MsgBox, MWAB = %MonitorWorkAreaBottom%
Return %MonitorWorkAreaBottom%


  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

; MsgBox, Monitor info: ( %TX% , %TY% , %TW% , %TH%, %TaskbarEdge%, %A_ScreenWidth%, %A_ScreenHeight% )

  if (TaskbarEdge = "top" or TaskbarEdge = "bottom") {
    SH = A_ScreenHeight - TH
    return SH
  } else {
    return A_ScreenHeight
  }
}

Max(a,b)
{
	if (a>=b)
		return a
	return b
}
Min(a,b)
{
	if (a<b)
		return a
	return b
}

Monitor_Info_Display(MonitorName, ByRef MWA)
{
    MsgBox, Monitor:`t#%A_Index%`nName:`t%MonitorName%
		. `nLeft:`t%MonitorLeft% (%MWALeft% work)
		. `nTop:`t%MonitorTop% (%MWATop% work)
		. `nRight:`t%MonitorRight% (%MWARight% work)
		. `nBottom:`t%MonitorBottom% (%MWABottom% work)
}


Monitor_Stats_Display()
{
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors
	MsgBox, There are %Num_Monitors% monitor(s)
	Loop, %Num_Monitors%
	{
	    SysGet, MonitorName, MonitorName, %A_Index%
	    SysGet, Monitor, Monitor, %A_Index%
	    SysGet, MWA, MonitorWorkArea, %A_Index%

if( false )
	    Monitor_Info_Display(MonitorName, MWA)

	    MsgBox, Monitor:`t#%A_Index%`nName:`t%MonitorName%
			. `nLeft:`t%MonitorLeft% (%MWALeft% work)
			. `nTop:`t%MonitorTop% (%MWATop% work)
			. `nRight:`t%MonitorRight% (%MWARight% work)
			. `nBottom:`t%MonitorBottom% (%MWABottom% work)
	}
}

Which_Monitor_am_I_in(x,y)
{
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors
	Base_x := 0
	Base_y := 0
	where_am_I := 0
	Loop, %Num_Monitors%
	{
		SysGet, MWA, MonitorWorkArea, %A_Index%
		if ( (x >= MWALeft ) && (x <= MWARight ) )
		{
			where_am_I := A_Index			
		}		
	}
	MsgBox, There are %Num_Monitors% monitor(s) `n Coordinates (%x%,%y%) are in monitor number %where_am_I%
	return where_am_I
}

GetMonitorAt(x, y, default=1) 
{ 
    SysGet, m, MonitorCount 
    ; Iterate through all monitors. 
    Loop, %m% 
    {   ; Check if the window is on this monitor. 
        SysGet, Mon, Monitor, %A_Index% 
        if (	x >= MonLeft 
        	&&	x <= MonRight
        	&&	y >= MonTop
        	&&	y <= MonBottom ) 
            return A_Index 
    } 
    return default 
}

Win_Move_Resize(Nx=0,Ny=0,w=0,h=0,mode="absolute")
{
	;------------------------------------------------------------------------------------------------------------------
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors

;	SysGet, MWA1, MonitorWorkArea, 1   ; Gets Monitor Work Area of display monitor #1
;	SysGet, MWA2, MonitorWorkArea, 2   ; Gets Monitor Work Area of display monitor #2
;	MsgBox, Monitor 1 work Area: %MWA1Left% -- Top: %MWA1Top% -- Right: %MWA1Right% -- Bottom %MWA1Bottom%.
;	MsgBox, Monitor 2 work Area: %MWA2Left% -- Top: %MWA2Top% -- Right: %MWA2Right% -- Bottom %MWA2Bottom%.

;	SysGet, Mon1, Monitor, 1 ; gets the dimensions of display monitor #1
;	SysGet, Mon2, Monitor, 2 ; gets the dimensions of display monitor #2
;	MsgBox, The monitor(s) dimensions Left: %Mon2Left% -- Top: %Mon2Top% -- Right: %Mon2Right% -- Bottom %Mon2Bottom%.
	;------------------------------------------------------------------------------------------------------------------

;listvars

	WinGetPos, X1, Y1, W1, H1, A
;	ScreenX := GetScreenLeft()
;	ScreenY := GetScreenTop()
;	ScreenW := GetScreenWidth()  ; #### * Num_Monitors
;	ScreenH := GetScreenHeight()

    Monitor_From := GetMonitorAt(X1+1,Y1+1) ; get monitor number based on top left corner of window
	SysGet, MonitorName, MonitorName, %Monitor_From%
	SysGet, MWA1, MonitorWorkArea, %Monitor_From%  ; Gets Monitor Work Area of current monitor
	Screen1X := MWA1Left
	Screen1Y := MWA1Top
	Screen1W := MWA1Right  - MWA1Left
	Screen1H := MWA1Bottom - MWA1Top

if (false) ; turn this to true for debugging purposes only
	MsgBox, Monitor work Area:
		. `n Left: %MWA1Left%
		. `n Top: %MWA1Top%
		. `n Right: %MWA1Right%
		. `n Bottom: %MWA1Bottom%


;	SysGet, DisplayMaxW, SM_CXVIRTUALSCREEN
;	SysGet, DisplayMaxH, SM_CYVIRTUALSCREEN
;	Screen1W := Min( DisplayMaxW, ScreenW )
;	Screen1H := Min( DisplayMaxH, ScreenH )

	if ( "relative" = mode )
	{
		W2 := W1 ; moving only - keep size
		H2 := H1 ; moving only - keep size

		X2 := X1 + Nx * Screen1W
		Y2 := Y1 + Ny * Screen1H
		Monitor_To := GetMonitorAt(X2, Y2, -1 )

		Monitor_To_BottomRight := GetMonitorAt(X2+W2, Y2+H2, -1 ) ; Monitor number where window bottom right corner would land
		SysGet, MWA2, MonitorWorkArea, %Monitor_To_Bottom_Right%  ; Gets Monitor Work Area of bottom right corner
		Screen2X := MWA2Left
		Screen2Y := MWA2Top
		Screen2W := MWA2Right - MWA2Left
		Screen2H := MWA2Bottom - MWA2Top

		if (-1 = Monitor_To)
		{ 	;------ Keep inside top-left monitor ------
			X2 := Max( Screen1X, X2 )
			Y2 := Max( Screen1Y, Y2 )
		}

		if ( (Monitor_From <> Monitor_To) && (X1 <> Screen1X) )
		{
			X2 := Max( X2, Screen1X ) ; snap to monitor left edge
		}

		if (Monitor_From <> Monitor_To && Y2 <> Screen1Y )
		{
			Y2 := Max( Y2, Screen1Y ) ; snap to monitor top edge
		}

		if (-1 = Monitor_To_BottomRight )
		{ 	;------ Do not move too far down-right ------
			X2 := Min( MWA2Right -W2, X2 )
			Y2 := Min( MWA2Bottom-H2, Y2 )
		}
	}

	if ( "absolute" = mode )
	{
		X2 := Screen1X + Nx * Screen1W / 2
		Y2 := Screen1Y + Ny * Screen1H / 2
		W2 := w * Screen1W / 2
		H2 := h * Screen1H / 2
	}

	if ( "resize" = mode )
	{
		X2 := X1 ; do not move upper-left corner
		Y2 := Y1 ; do not move upper-left corner

		Monitor_From_BottomRight := GetMonitorAt( X2+W1, Y2+H1, -1 ) ; Monitor number where window bottom right corner currently lands

		dW := Screen1W
		dH := Screen1H

		W2 := W1 + w * dW
		H2 := H1 + h * dH

		Monitor_To_BottomRight   := GetMonitorAt( X2+W2, Y2+H2, -1 ) ; Monitor number where window bottom right corner WOULD land

		if ( Monitor_To_BottomRight = -1 )
			Monitor_To_BottomRight := Monitor_From_BottomRight ; do not resize into non-existing monitor

		SysGet, MWA2, MonitorWorkArea, %Monitor_To_BottomRight%   ; Gets Monitor Work Area of bottom right corner
		Screen2X := MWA2Left
		Screen2Y := MWA2Top
		Screen2W := MWA2Right  - MWA2Left
		Screen2H := MWA2Bottom - MWA2Top

		if( Monitor_To_BottomRight <> Monitor_From_BottomRight )
		{
			dW := Screen2W
			dH := Screen2H

			W2 := W1 + w * dW
			H2 := H1 + h * dH
		}

		W2 := Max( 0.1*dW, W1 + w * dW )
		H2 := Max( 0.1*dH, H1 + h * dH )

		if( Monitor_To_BottomRight = Monitor_From )
		{
			W2 := Min( W2, MWA1Right - X2 )
			H2 := Min( H2, MWA1Bottom- Y2 )
		}
		else
		{
			W2 := Min( W2, MWA2Right  -X2 )
			H2 := Min( H2, MWa2Bottom -Y2 )
		}

	}

if (false) ; turn this to true for debugging purposes only
{
	MsgBox, Monitor work Area:
		. `n Monitor_from %Monitor_From%
		. `n Left: %MWA1Left%
		. `n Top: %MWA1Top%
		. `n Right: %MWA1Right%
		. `n Bottom: %MWA1Bottom%
		. `n Screen1X: %Screen1X%
		. `n Screen1Y: %Screen1Y%
		. `n Screen1W: %Screen1W%
		. `n Screen1H: %Screen1H%
		. `n -----------------------
		. `n Monitor_to %Monitor_To_BottomRight%
		. `n Left: %MWA2Left%
		. `n Top: %MWA2Top%
		. `n Right: %MWA2Right%
		. `n Bottom: %MWA2Bottom%
		. `n Screen2X: %Screen2X%
		. `n Screen2Y: %Screen2Y%
		. `n Screen2W: %Screen2W%
		. `n Screen2H: %Screen2H%
		. `n -----------------------
		. `n Current Window - Before
		. `n win x1: %X1%
		. `n win y1: %Y1%
		. `n win w1: %W1%
		. `n win h1: %H1%
		. `n Current Window - After
		. `n win x2: %X2%
		. `n win y2: %Y2%
		. `n win w2: %W2%
		. `n win h2: %H2%
		. `n -----------------------
		. `n dW: %dW%
		. `n dH: %dH%
}

	WinMove, A, , X2, Y2, W2, H2
	return
}


;#######################################################################
; Some test functions to understand how to deal with multiple monnitors
;#######################################################################
#^+S::Monitor_Stats_Display()
#^+w::Which_Monitor_am_I_in(1000,1000)
#^+e::Which_Monitor_am_I_in(2000,1000)
#^+r::Which_Monitor_am_I_in(3000,1000)
#^+t::Which_Monitor_am_I_in(4000,2000)
;#######################################################################

#Numpad1::Win_Move_Resize(0,1,1,1)
#Numpad2::Win_Move_Resize(0,1,2,1)
#Numpad3::Win_Move_Resize(1,1,1,1)
#Numpad4::Win_Move_Resize(0,0,1,2)
#Numpad5::Win_Move_Resize(0,0,2,2)
#Numpad6::Win_Move_Resize(1,0,1,2)
#Numpad7::Win_Move_Resize(0,0,1,1)
#Numpad8::Win_Move_Resize(0,0,2,1)
#Numpad9::Win_Move_Resize(1,0,1,1)


#^+Down::Win_Move_Resize( 0,0, 0.0, 0.1,"resize")
#^+Up::Win_Move_Resize(   0,0, 0.0,-0.1,"resize")
#^+Left::Win_Move_Resize( 0,0,-0.1, 0.0,"resize")
#^+Right::Win_Move_Resize(0,0, 0.1, 0.0,"resize")

#^Up::Win_Move_Resize(    0 , -0.1,0,0,"relative")
#^Down::Win_Move_Resize(  0 ,  0.1,0,0,"relative")
#^Left::Win_Move_Resize( -0.1,   0,0,0,"relative")
#^Right::Win_Move_Resize( 0.1,   0,0,0,"relative")


;#######################################################################
;The shortcuts below are intended for save-as operations with acrobat,
; where the file name has a pattern stored in the clipboard
;#######################################################################
#a::
Send ^+s
Sleep, 200
Send ^v
Sleep, 100
Send {Right}
;Sleep, 100
Send ^{Left}
;Sleep 100
Send {Left}
return

#s::
Send ^a^c!s
Sleep, 100
Send ^w
return
;#######################################################################


;GetActiveWindowInfo()
;{
	; Returns an object containing information on the active window
	; Specifically:
	;   - X coordinate
	;   - Y coordinate
	;   - Window width
	;   - Window height
	;   - Window state (-1 = minimize, 0 = neither minimized nor maximized, 1 = maximized)
	;   - Monitor (the monitor number the center of the window lies in)
;	WinGetPos, X, Y, W, H, A
;	WinGet, windowState, MinMax, A
;	Monitor := GetMonitorAt(X + W/2, Y + H/2)
; 	return Object("X", X
;                 ,"Y", Y
;                 ,"W", W
;                 ,"H", H
;                 ,"Monitor", Monitor
;                 ,"windowState", windowState
;                 ,"newMonitor", Monitor)
;}


Get_Info() {
  	SysGet, numberOfMonitors, MonitorCount

	SysGet, MonitorWorkArea, MonitorWorkArea, 0

;	winInfo := GetActiveWindowInfo()
;	MsgBox winInfo

;	SysGet, monArea, Monitor, %A_Index%
;	MsgBox, Monitor number %A_Index%

	WinGetPos, X1, Y1, W1, H1, A
    Monitor := GetMonitorAt(X1,Y1)

	WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
	TaskbarEdge := GetTaskbarEdge()

	MsgBox, Window coordinates:
		. `n %X1%, %Y1%, %W1%, %H1%
		. `n Monitor # %Monitor%
		. `n
		. `n 'Monitor Info:'
		. `n %TX%, %TY%, %TW%, %TH%
		. `n TaskBar Hedge = %TaskbarEdge%
		. `n Screen Width  = %A_ScreenWidth%
		. `n Screen Height =  %A_ScreenHeight%
		. `n Monitor Work Area Bottom = %MonitorWorkAreaBottom%
		. `n Monitor Work Area    Top = %MonitorWorkAreaTop%
		. `n Monitor Work Area   Left = %MonitorWorkAreaLeft%
		. `n Monitor Work Area  Right = %MonitorWorkAreaRight%


;	listvars
}

#^+i::Get_Info()

#!e::Run C:\Users\roca4364\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\This PC.lnk

;#######################################################################
; USEFUL FOR OUTLOOK
;#######################################################################

#ifWinActive, ahk_exe outlook.exe
{ 
	^+!v::Send !H!V!T
}

Paste_ClipText() {
	ClipSaved := ClipboardAll ; save clipboard
	clipboard = %clipboard%   ; Convert any copied files, HTML, or other formatted text to plain text.
	Send ^v
	Clipboard := ClipSaved   ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
}

^+v::Paste_ClipText()

