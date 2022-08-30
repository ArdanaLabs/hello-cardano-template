const axios = require('axios').default;

exports.sign = url => value => async () =>
    await axios.post('/sign', {
        params: {
            value 
        }
    })

exports.getPubKey = url => value => async () =>
    await axios.get('/pubkey')
