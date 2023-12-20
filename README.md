## INSTRUCCIONES DE USO: SERVIDOR PROLOG

- Lanzar usando "run_server.bat".
- Lanzar peticiones al servidor usando POSTMAN, CURL o software equivalente

# Ejemplo de solicitud POST a la ruta /consultar
curl -X POST -H "Content-Type: application/json" -d '{"caracteres": ["a", "b", "c"], "longitud": 5}' http://localhost:3050/consultar
