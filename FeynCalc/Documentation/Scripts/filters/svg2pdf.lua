-- https://groups.google.com/g/pandoc-discuss/c/w5Uxh02YE4E/m/QEjz3vr9AgAJ
local utils = require 'pandoc.utils'
function Para (figure)
      if #figure.content == 1 and figure.content[1].t == 'Image' then
         local img = figure.content[1]
            img.src = string.gsub(img.src, ".svg", ".pdf")
            return pandoc.Plain {
                     pandoc.RawInline('tex',"\\begin{figure}[!ht]\n"),
                     pandoc.RawInline('tex','\\centering\n'),
                     pandoc.RawInline('tex',string.format("\\includegraphics[width=0.6\\linewidth]{%s}\n",img.src)),
                     pandoc.RawInline('tex',"\\end{figure}\n")
            }        
      end
end
