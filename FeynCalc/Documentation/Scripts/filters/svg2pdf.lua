-- https://groups.google.com/g/pandoc-discuss/c/w5Uxh02YE4E/m/QEjz3vr9AgAJ
  function Figure(figure)
	img = pandoc.utils.stringify(figure.content[1])
	
	return pandoc.Plain {
                     pandoc.RawInline('tex',"\\FloatBarrier\n"),
                     pandoc.RawInline('tex',"\\begin{figure}[!ht]\n"),
                     pandoc.RawInline('tex','\\centering\n'),
                     pandoc.RawInline('tex',string.format("\\includegraphics[width=0.6\\linewidth]{img/%s.pdf}\n",img)),
                     pandoc.RawInline('tex',"\\end{figure}\n"),
                     pandoc.RawInline('tex',"\\FloatBarrier\n")
            }  
	
end
