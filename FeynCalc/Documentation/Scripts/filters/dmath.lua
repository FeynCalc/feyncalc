-- https://stackoverflow.com/questions/56828128/pandoc-is-tex-output-with-dollar-sign-math-possible
-- Do nothing unless we are targeting TeX.
if not FORMAT:match('tex$') then return {} end

function Math (m)
  --local delimiter = m.mathtype == '$$' and '$' or '$$'
  if m.mathtype == "DisplayMath" then
  --return pandoc.RawInline('tex', '\\begin{dgroup}\\begin{dmath}' .. m.text .. '\\end{dmath}\\end{dgroup}')
  return pandoc.RawInline('tex', '\\begin{dmath*}\\breakingcomma\n' .. m.text .. '\n\\end{dmath*}')
  else
  return m
  end
end
