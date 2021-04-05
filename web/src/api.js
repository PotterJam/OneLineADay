export { getData, postData }

const fetchMode = 'cors'; // for prod release use 'cors' or 'same-origin'
const credentialsOption = 'same-origin'; // for prod release might need this to include for a different sub-domain

const getData = async (url = '') => {
    const response = await fetch(url, {
      method: 'GET',
      mode: fetchMode,
      cache: 'no-cache',
      credentials: credentialsOption,
      headers: {
        'Content-Type': 'application/json'
      },
    });
    return response.json();
}

const postData = async (url = '', data = {}) => {
    const response = await fetch(url, {
      method: 'POST',
      mode: fetchMode,
      cache: 'no-cache',
      credentials: credentialsOption,
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(data)
    });
    return response.json();
}