# Guikell
Simple language for defining UI in different frontends

## Example
```c
win 
  title "Título da aplicaçom"

  draw -> ("fgColor" "#FF0000")
    rect 
      at 150 50
      sized 50 150

    rect
      at 50 50
      sized 150 50
  end
end
```
