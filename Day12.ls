_ = require 'lodash'
fs = require 'fs'

class Red

sum = (doc)->
  | _.isArray doc => _.sum doc, -> sum it
  | _.isObject doc =>
    try
      (_.sum doc, -> (if it=='red' then throw new Red else sum it))
    catch
      0
  | _.isNumber doc => doc
  | otherwise => 0

console.log sum JSON.parse fs.readFileSync './Day12.in.txt'
