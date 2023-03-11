# Guikell
Simple language for defining UI in different frontends

## Example
```c
win 
  title "Título da aplicaçom"

  draw -> ("fgColor" "#008080")
    rect
      at 50 50
      sized 150 50
  end

  draw -> ("fgColor" "#8A2BE2")
    text
      at 90 80
      as "Hello, world"
  end
end
```
