return {
  {
    Link = function (li)
      local target
      local name = li.target      
      if (name == "Extra/FeynCalc.md") or (name == "FeynCalc.md") or (name == "Extra/FeynHelpers.md") or (name == "FeynHelpers.md") then
         name ="Overview"
         target = "toc"
      elseif string.sub(name,1, 4) == "http" then
         return li
      else
        --print(name," ",target)
        name = string.gsub(name, '.md', '')        
        target = name:lower()        
        name = string.gsub(name, '%$', '\\$')
        target = string.gsub(target, '%$', 'dollar')        
      end
      
      return pandoc.RawInline('tex', '\\hyperlink{' .. target  .. '}{' .. name .. '}')
    end
 }
}


