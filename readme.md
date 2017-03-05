# Include files


```
moren/
+-- src/moren.lisp    
+-- src/man.lisp      
+-- css/moren.css 
+-- css/console.css 
L-- lib/moren.js   
```

# Moren concept

IDE Moren created for rapidly programming and prototyping programs on JSCL language (subset of Common Lisp) in yours browser.

>Moren - reduction of the from moraine-morena (geological phenomenon).
The ancient moraines, formed the geological landscape of the contemporary world.
Lisp,  also, as an ancient moraines, has identified the tectonics of the modern programming languages. 
When we see the increasingly popular programming language, we see in him the contours of Lisp tectonics.

IDE contains all the minimum necessary functions.

Other additions may be to developed and debugged in Moren IDE, compiled in JSCL and can be added "on the fly".

It has the property of:

- complete openness
- manageability
- extensibility
- customization capabilities for any user requirement

Moren IDE is not intended for:

- industrial and/or conveyor production. 


>Intended only for research, rapid development and prototyping lisp programm in the browser environment.



Integration is made based on the following components:

- `JSCL` Subset of Common Lisp
- `JQConsole`  Interactivity
- `CodeMirror` Built-in editor
- `Klib`  Interaction with the browser API's and the Document Object Model
- `Moren` Some integration environment


Text editing options:

- line editor `JQConsole`
- builtin `CodeMirror` editor in emacs configuration

Download text files for compilation in an environment:

- Implementation of the load function

Connection compiled in JSCL additions, and other JS libraries, images and css resources is performed on the fly.
Just call lisp functions.


The available area of the screen is divided into panels:

- `Banner` for output service IDE messages
- `Control` for the location of controls (buttons, menus)
- `Console` for interactive interaction with the environment 

*see bellow*

```
+-----------------------------------------------+
|               banner                          |
+-----------------------------------------------+
|               control                         |
+-----------------------------------------------+
|                                               |
|               console                         |
|                                               |
+-----------------------------------------------+
```

Positioning of the panels in the browser screen coordinates absolute. Panel dimensions, colors defined CSS
See details in [./moren/css/moren.css]

General view See  [./moren/moren-ide.png]


## Moren IDE fuctions

Function **BANNER-MSG**

#### Syntax:

**banner-msg** *`style`* *`msg`* => *`result`*

#### Arguments and Values:

*`style`* is the number of one of the styles:

- 0  `<strong  style="color:red;"</strong>`
- 1  `<font style="color:red"></font>`
- 2  `<strong style="color:orange;"></strong>`
- 3  `<font style="color:orange"></font>`
- 4  `<strong style="color:green;"></strong>`
- 5  `<font style="color:green"></font>`

*`msg`* any string - "CLR #(0 0 1)". Acceptable use of tags.

*`result`* *none*

#### Description:

Function display message in a **banner** ide field. For output, used **dom-set-inner-html**. 
Therefore any text, available in this field will be overwritten.

#### Examples:

```lisp
(if (> rc 0)
    (banner-msg 2 (format nil "CLR ~a~%" droped)))
```



Function **take-console-history**

#### Syntax:

**take-console-history**  => *`result`*

#### Arguments and Values:

*`none`*
*`result`* *none*

#### Description:

Function retrieve the history of commands from the storage and makes it available for viewing and manipulation 
with commands **look-console-history** **take-history-item**.

It displays the number of stored history records in the **banner** filed IDE as (possible) "History has 318 items"

#### Examples:

```lisp
        (take-console-history)
```


Function **look-console-history**

#### Syntax:

**look-console-history** &optional *`from`* *`to`* => *`result`*

#### Arguments and Values:

*`from`* first history item (optionaly 0)

*`to`* last history item (optionaly 5)

*`result`* *none*

#### Description:

Function prints console history from *`from`* to *`to`* range on console.
Each record of the history of this range is printed in the format:
```
     item-number : "string with entered command, stored under item-number". 
```

#### Examples:

```lisp
(look-console-history)
=>
; 0: "(man 'man')"
; 1: ""
; 2: "(load 'j/addon-edit.lisp')"
; 3: ""
; 4: "(edit)"
; 5: "(cm-hide t)"
```

Function **take-history-item**

#### Syntax:

**take-history-item** *`item`* => *`result`*

#### Arguments and Values:

*`item`* history item number

*`result`* *none*

#### Description:

Function retrieves command with the specified number from history, and inserts her into the input field. 
Makes her available for editing and execution

#### Examples:

```lisp
(take-history-item 4)
```

Function **reset-btn-on**

#### Syntax:


**reset-btn-on**  => *`result`*

#### Arguments and Values:

*`none`*

*`result`* *none*

#### Description:
Large history of working with the console slow down your browser. 
Function makes available "RST" button on the IDE control panel.
After this, clicking on button "RST" will result in full removal of the console history from a browser.

#### Examples:

```lisp
(reset-btn-on)
```

Function **reset-btn-off**

#### Syntax:

**reset-btn-off**  => *`result`*

#### Arguments and Values:

*`none`*

*`result`* *none*

#### Description:

Remove button "RST" from IDE control panel.

#### Examples:

```lisp
(reset-btn-off)
```


