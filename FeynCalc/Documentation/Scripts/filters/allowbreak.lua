return {
  {
    Inline = function (elem)
      if elem.tag == "Code" then
        local new = elem.text
      new = string.gsub(new, '%{', '\\{\\allowbreakXXXXXX')
      new = string.gsub(new, '%}', '\\}')
      new = string.gsub(new, 'XXXXXX', '{}')
      new = string.gsub(new, ', ', ',')
      new = string.gsub(new, '%[', '[\\allowbreak{}')      
      new = string.gsub(new, ',', ',\\ \\allowbreak{}')
      new = string.gsub(new, '#', '\\#{}\\allowbreak{}')
      new = string.gsub(new, '&', '\\&{}\\allowbreak{}')
      new = string.gsub(new, '%$', '\\$')
      return pandoc.RawInline('tex', '\\texttt{' .. new  .. '}')
      end
    end
  }
}
