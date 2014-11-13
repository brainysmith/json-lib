package com.identityblitz.jwt

import java.math.BigInteger

import com.identityblitz.json.JVal
import org.apache.commons.codec.binary.Hex
import org.scalatest.{FlatSpec, Matchers}

class JwkTest extends FlatSpec with Matchers {

  object BaseJwtToolkit extends AlgorithmsKit with JwsToolkit with SimpleCryptoService with SimpleKidsRegisterService
  import BaseJwtToolkit._

  behavior of "JWK deserialization"

  it should "deserialize EC public key" in {
    val ecPubKeyJwkStr = """{"kty":"EC",
                      "crv":"P-256",
                      "x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4",
                      "y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM",
                      "use":"enc",
                      "kid":"1"}"""

    val ecPubKeyJwk = JVal.parse(ecPubKeyJwkStr).as[JWK]

    ecPubKeyJwk should be (a [EcPublicKey])
    ecPubKeyJwk.kty should be ("EC")
    ecPubKeyJwk.use should be (Some(Use.enc))
    ecPubKeyJwk.kid should be (Some("1"))


    ecPubKeyJwk.asInstanceOf[EcPublicKey].crv should be ("P-256")
    ecPubKeyJwk.asInstanceOf[EcPublicKey].x.deep should be (Hex.decodeHex("30a0424cd21c2944838a2d75c92b37e76ea20d9f00893a3b4eee8a3c0aafec3e".toCharArray).deep)
    ecPubKeyJwk.asInstanceOf[EcPublicKey].y.deep should be (Hex.decodeHex("e04b65e92456d9888b52b379bdfbd51ee869ef1f0fc65b6659695b6cce081723".toCharArray).deep)
  }

  it should "deserialize RSA public key" in {
    val rsaPubKeyJwkStr = """{"kty":"RSA",
                              "n": "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw",
                              "e":"AQAB",
                              "alg":"RS256",
                              "kid":"2011-04-29"}"""

    val rsaPubKeyJwk = JVal.parse(rsaPubKeyJwkStr).as[JWK]

    rsaPubKeyJwk should be (a [RsaPublicKey])
    rsaPubKeyJwk.kty should be ("RSA")
    rsaPubKeyJwk.alg should be (Some("RS256"))
    rsaPubKeyJwk.kid should be (Some("2011-04-29"))

    rsaPubKeyJwk.asInstanceOf[RsaPublicKey].e should be (BigInteger.valueOf(65537L))

    rsaPubKeyJwk.asInstanceOf[RsaPublicKey].n should be (new BigInteger(
      "26634547600177008912365441464036882611104634136430581696102639463075266" +
        "43621694631605384564230016632004291503192450127270527504313021178322825" +
        "23691948569493977828808472351433815292073822626479069876557386473870073" +
        "20361149854766523417293323739185308113373529512728932838100141612048712" +
        "59717869572065134429545017489536992338339670433433162726156590726674986" +
        "37447079206063646782316391064038549773021837192462569585506515557676641" +
        "34467706614553219592981545363271425781391262006405169505726523023628770" +
        "28543206204439131004744574928756316166854835432256022350994699082769165" +
        "4627968182167826397015368836435965354956581554819", 10))
  }

  it should "deserialize EC private key" in {
    val ecPrivKeyJwkStr = """{"kty":"EC",
                              "crv":"P-256",
                              "x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4",
                              "y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM",
                              "d":"870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE",
                              "use":"enc",
                              "kid":"1"}"""

    val ecPrivKeyJwk = JVal.parse(ecPrivKeyJwkStr).as[JWK]

    ecPrivKeyJwk should be (a [EcPrivateKey])
    ecPrivKeyJwk.kty should be ("EC")
    ecPrivKeyJwk.use should be (Some(Use.enc))
    ecPrivKeyJwk.kid should be (Some("1"))

    ecPrivKeyJwk.asInstanceOf[EcPrivateKey].crv should be ("P-256")
    ecPrivKeyJwk.asInstanceOf[EcPrivateKey].x.deep should be (Hex.decodeHex("30a0424cd21c2944838a2d75c92b37e76ea20d9f00893a3b4eee8a3c0aafec3e".toCharArray).deep)
    ecPrivKeyJwk.asInstanceOf[EcPrivateKey].y.deep should be (Hex.decodeHex("e04b65e92456d9888b52b379bdfbd51ee869ef1f0fc65b6659695b6cce081723".toCharArray).deep)
    ecPrivKeyJwk.asInstanceOf[EcPrivateKey].d.deep should be (Hex.decodeHex("f3bd0c07a81fb932781ed52752f60cc89a6be5e51934fe01938ddb55d8f77801".toCharArray).deep)


  }

  it should "deserialize RSA private key" in {
    val rsaPrivKeyJwkStr = """{"kty":"RSA",
                               "n":"0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw",
                               "e":"AQAB",
                               "d":"X4cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9M7dx5oo7GURknchnrRweUkC7hT5fJLM0WbFAKNLWY2vv7B6NqXSzUvxT0_YSfqijwp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdYaMwFs9tv8d_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBznbJSzFHK66jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFzme1z0HbIkfz0Y6mqnOYtqc0X4jfcKoAC8Q",
                               "p":"83i-7IvMGXoMXCskv73TKr8637FiO7Z27zv8oj6pbWUQyLPQBQxtPVnwD20R-60eTDmD2ujnMt5PoqMrm8RfmNhVWDtjjMmCMjOpSXicFHj7XOuVIYQyqVWlWEh6dN36GVZYk93N8Bc9vY41xy8B9RzzOGVQzXvNEvn7O0nVbfs",
                               "q":"3dfOR9cuYq-0S-mkFLzgItgMEfFzB2q3hWehMuG0oCuqnb3vobLyumqjVZQO1dIrdwgTnCdpYzBcOfW5r370AFXjiWft_NGEiovonizhKpo9VVS78TzFgxkIdrecRezsZ-1kYd_s1qDbxtkDEgfAITAG9LUnADun4vIcb6yelxk",
                               "dp":"G4sPXkc6Ya9y8oJW9_ILj4xuppu0lzi_H7VTkS8xj5SdX3coE0oimYwxIi2emTAue0UOa5dpgFGyBJ4c8tQ2VF402XRugKDTP8akYhFo5tAA77Qe_NmtuYZc3C3m3I24G2GvR5sSDxUyAN2zq8Lfn9EUms6rY3Ob8YeiKkTiBj0",
                               "dq":"s9lAH9fggBsoFR8Oac2R_E2gw282rT2kGOAhvIllETE1efrA6huUUvMfBcMpn8lqeW6vzznYY5SSQF7pMdC_agI3nG8Ibp1BUb0JUiraRNqUfLhcQb_d9GF4Dh7e74WbRsobRonujTYN1xCaP6TO61jvWrX-L18txXw494Q_cgk",
                               "qi":"GyM_p6JrXySiz1toFgKbWV-JdI3jQ4ypu9rbMWx3rQJBfmt0FoYzgUIZEVFEcOqwemRN81zoDAaa-Bk0KWNGDjJHZDdDmFhW3AN7lI-puxk_mHZGJ11rxyR8O55XLSe3SPmRfKwZI6yU24ZxvQKFYItdldUKGzO6Ia6zTKhAVRU",
                               "alg":"RS256",
                               "kid":"2011-04-29"}"""

    val rsaPrivKeyJwk = JVal.parse(rsaPrivKeyJwkStr).as[JWK]

    rsaPrivKeyJwk should be (a [RsaPrivateKey])
    rsaPrivKeyJwk.kty should be ("RSA")
    rsaPrivKeyJwk.alg should be (Some("RS256"))
    rsaPrivKeyJwk.kid should be (Some("2011-04-29"))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].e should be (BigInteger.valueOf(65537L))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].n should be (new BigInteger(
      "26634547600177008912365441464036882611104634136430581696102639463075266" +
        "43621694631605384564230016632004291503192450127270527504313021178322825" +
        "23691948569493977828808472351433815292073822626479069876557386473870073" +
        "20361149854766523417293323739185308113373529512728932838100141612048712" +
        "59717869572065134429545017489536992338339670433433162726156590726674986" +
        "37447079206063646782316391064038549773021837192462569585506515557676641" +
        "34467706614553219592981545363271425781391262006405169505726523023628770" +
        "28543206204439131004744574928756316166854835432256022350994699082769165" +
        "4627968182167826397015368836435965354956581554819", 10))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].d should be (new BigInteger(
      "12059247920106998877834196630336549090121729843146690429352176950239290" +
        "49181173151099784490812781841119723847204320805445753736598872048222732" +
        "09416225794628908923420873706213216979137075831904320314434400855076471" +
        "03423659911812976629405447233056832745596117326566758077192204434965308" +
        "89262681291665604680224733118493889676917405444969435539120561957451257" +
        "17007674919620949283379368471306603650502403202666402170140235761137103" +
        "33420737911211020431514635349872426319659058955767361242629552548559609" +
        "52092502396939874671602685380711147625369418903380603762409153650222479" +
        "5278572727576279224349624427644439187009145144049", 10))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].p should be (Some(new BigInteger(
      "17097161624706688664994930073240072034521040965832746062526456906019557" +
        "03560752559950003264349576489333895498067702806232630466519149375336189" +
        "40627146749024663149056492969053735348931398424539579724444098289604654" +
        "24223317357580984244956820133838904364240123044349622906253148038313902" +
        "2143795969631711734885883", 10)))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].q should be (Some(new BigInteger(
      "15578344631010610463509916094678436382141485365496555049620723506404815" +
        "49178747827741564860758056504575930074171885258250456972528672226444003" +
        "61049116649131900972223186707453028717132200988108509861413777141915906" +
        "73241223460256620578455388888587006632393462329760186951268165280704854" +
        "5792342297108375492531993", 10)))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].dp should be (Some(new BigInteger(
      "19341495076914626815733465304027021997335703147944516732162160535457679" +
        "76288115031122774036328754763251522227546874071960682100000453708400217" +
        "93156486564424501058502042948649525287544041979269182916067037661035583" +
        "95286887542778188991273611009396468705689346819477257766904319629531298" +
        "505792198587813155964477", 10)))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].dq should be (Some(new BigInteger(
      "12629400622399938119485868319061904014824591588784922507002131019405744" +
        "11239697435581993112546138824551379232659817136214734110615696233626750" +
        "62680632569159818584237760851941450917343652756445873894849861808247769" +
        "05564481799088980392052020492843985067758625616712521063030484909732359" +
        "2573552933269223466037769", 10)))

    rsaPrivKeyJwk.asInstanceOf[RsaPrivateKey].qi should be (Some(new BigInteger(
      "19056734019852286198079387917604722000587388095884598882617821195913318" +
        "62269208348988554016957958110245045710050897926957938959373900134278359" +
        "25226379467723975575042560855020292521604351425161303112363756610923663" +
        "96531117762869783482123081301819792999713394710397682392804152123341375" +
        "290541162928420868347157", 10)))
  }

  it should "deserialize symmetric key" in {
    val symmetricKeyStr = """{"kty":"oct",
                              "alg":"A128KW",
                              "k":"GawgguFyGrWKav7AX4VKUg"}"""

    val symmetricKey = JVal.parse(symmetricKeyStr).as[JWK]

    symmetricKey should be (a [SymmetricKey])
    symmetricKey.kty should be ("oct")
    symmetricKey.alg should be (Some("A128KW"))
    symmetricKey.asInstanceOf[SymmetricKey].k.deep should be (Hex.decodeHex("19ac2082e1721ab58a6afec05f854a52".toCharArray))

    val hmacStr = """{"kty":"oct",
                      "k":"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow",
                      "kid":"HMAC key used in JWS A.1 example"}"""

    val hmacKey = JVal.parse(hmacStr).as[JWK]

    hmacKey should be (a [SymmetricKey])
    hmacKey.kty should be ("oct")
    hmacKey.kid should be (Some("HMAC key used in JWS A.1 example"))
    hmacKey.asInstanceOf[SymmetricKey].k.deep should be (Hex.decodeHex("0323354b2b0fa5bc837e0665777ba68f5ab328e6f054c928a90f84b2d2502ebfd3fb5a92d20647ef968ab4c377623d223d2e2172052e4f08c0cd9af567d080a3".toCharArray))


  }

  it should "deserialize a RSA public key with the X.509 certificate" in {
    val rsaPubKeyWithCertStr = """{"kty":"RSA",
                                   "use":"sig",
                                   "kid":"1b94c",
                                   "n":"vrjOfz9Ccdgx5nQudyhdoR17V-IubWMeOZCwX_jj0hgAsz2J_pqYW08PLbK_PdiVGKPrqzmDIsLI7sA25VEnHU1uCLNwBuUiCO11_-7dYbsr4iJmG0Qu2j8DsVyT1azpJC_NG84Ty5KKthuCaPod7iI7w0LK9orSMhBEwwZDCxTWq4aYWAchc8t-emd9qOvWtVMDC2BXksRngh6X5bUYLy6AyHKvj-nUy1wgzjYQDwHMTplCoLtU-o-8SNnZ1tmRoGE9uJkBLdh5gFENabWnU5m1ZqZPdwS-qo-meMvVfJb6jJVWRpl2SUtCnYG2C32qvbWbjZ_jBPD5eunqsIo1vQ",
                                   "e":"AQAB",
                                   "x5c":["MIIDQjCCAiqgAwIBAgIGATz/FuLiMA0GCSqGSIb3DQEBBQUAMGIxCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYDVQQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1wYmVsbDAeFw0xMzAyMjEyMzI5MTVaFw0xODA4MTQyMjI5MTVaMGIxCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJDTzEPMA0GA1UEBxMGRGVudmVyMRwwGgYDVQQKExNQaW5nIElkZW50aXR5IENvcnAuMRcwFQYDVQQDEw5CcmlhbiBDYW1wYmVsbDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL64zn8/QnHYMeZ0LncoXaEde1fiLm1jHjmQsF/449IYALM9if6amFtPDy2yvz3YlRij66s5gyLCyO7ANuVRJx1NbgizcAblIgjtdf/u3WG7K+IiZhtELto/A7Fck9Ws6SQvzRvOE8uSirYbgmj6He4iO8NCyvaK0jIQRMMGQwsU1quGmFgHIXPLfnpnfajr1rVTAwtgV5LEZ4Iel+W1GC8ugMhyr4/p1MtcIM42EA8BzE6ZQqC7VPqPvEjZ2dbZkaBhPbiZAS3YeYBRDWm1p1OZtWamT3cEvqqPpnjL1XyW+oyVVkaZdklLQp2Btgt9qr21m42f4wTw+Xrp6rCKNb0CAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAh8zGlfSlcI0o3rYDPBB07aXNswb4ECNIKG0CETTUxmXl9KUL+9gGlqCz5iWLOgWsnrcKcY0vXPG9J1r9AqBNTqNgHq2G03X09266X5CpOe1zFo+Owb1zxtp3PehFdfQJ610CDLEaS9V9Rqp17hCyybEpOGVwe8fnk+fbEL2Bo3UPGrpsHzUoaGpDftmWssZkhpBJKVMJyf/RuP2SmmaIzmnw9JiSlYhzo4tpzd5rFXhjRbg4zW9C+2qok+2+qDM1iJ684gPHMIY8aLWrdgQTxkumGmTqgawR+N5MDtdPTEQ0XfIBc2cJEUyMTY5MPvACWpkA6SdS4xSvdXK3IVfOWA=="]
                                  }"""

    val rsaPubKeyWithCert = JVal.parse(rsaPubKeyWithCertStr).as[JWK]

  }


}
