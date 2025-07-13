let a = {
  type: 'object',
  properties: { node: { '$ref': '#/$defs/Node' } },
  additionalProperties: true,
  required: [ 'node' ],
  '$defs': {
    Node: {
      type: 'object',
      properties: {
        Id: { type: 'string' },
        Children: { items: {...}, type: 'array' }
      },
      additionalProperties: true,
      required: [ 'Id', 'Children' ]
    }
  }
}