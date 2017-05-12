'use strict';

exports.randomUUID4 = function() {
  var bytes = new Uint8Array(16);
  crypto.getRandomValues(bytes);
  bytes[6] = bytes[6] & 0x0f | (4 << 4);
  bytes[8] = bytes[8] & 0x3f | 0x80;
  return Array.from(bytes).map(function(byte, idx) {
    return (idx == 4 || idx == 6 || idx == 8 || idx == 10 ? '-' : '') +
            ('0' + byte.toString(16)).slice(-2);
  }).join('');
};
