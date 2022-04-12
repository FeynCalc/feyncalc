return {
  {
    Link = function (li)
      local target
      local name = li.target      
      if (name == "Extra/FeynCalc.md") or (name == "FeynCalc.md") then
         name ="Overview"
         target = "toc"
      elseif string.sub(name,1, 4) == "http" then
         return li
      else
        --print(name," ",target)
        name = string.gsub(name, '.md', '')
        target = name:lower()
        name = string.gsub(name, '%$', '\\$')
        target = string.gsub(target, '%$', '')
        target = string.gsub(target, '.md', '')
      end      
      return pandoc.RawInline('tex', '\\hyperlink{' .. target  .. '}{' .. name .. '}')
    end
 }
}


