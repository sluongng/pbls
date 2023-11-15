fn main() -> pbls::Result<()> {
    let (connection, io_threads) = lsp_server::Connection::stdio();
    pbls::run(connection)?;
    io_threads.join()?;
    Ok(())
}
