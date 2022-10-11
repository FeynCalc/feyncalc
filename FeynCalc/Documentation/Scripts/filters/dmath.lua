-- https://stackoverflow.com/questions/56828128/pandoc-is-tex-output-with-dollar-sign-math-possible
-- Do nothing unless we are targeting TeX.
if not FORMAT:match('tex$') then return {} end

function Math (m)
  --local delimiter = m.mathtype == '$$' and '$' or '$$'
  if m.mathtype == "DisplayMath" then
    if string.find(m.text, "\\begin{align}") or string.find(m.text, "\\begin{split}") then
        return pandoc.RawInline('tex', m.text)
     else
       return pandoc.RawInline('tex', '\\begin{dmath*}\\breakingcomma\n' .. m.text .. '\n\\end{dmath*}')
    end
  else
  return m
  end
end
