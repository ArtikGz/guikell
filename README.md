# Guikell
Simple language for defining UI in different frontends

## Build & Run
For running simply do:
```shell
cabal run
```

## Example
```c
win 
  title "Título da aplicaçom"

  draw -> ("fgColor" "#008080")
    rect
      sized 150 50
      at 50 50
  end

  draw -> ("fgColor" "#8A2BE2")
    rect
      at 55 55
      sized 140 40

    text
      at 90 80
      as "Hello, world"
  end
end
```
